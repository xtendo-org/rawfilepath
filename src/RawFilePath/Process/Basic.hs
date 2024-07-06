module RawFilePath.Process.Basic where

-- base modules

import RawFilePath.Import hiding (ClosedHandle)
-- local modules

import RawFilePath.Process.Common
import RawFilePath.Process.Internal
import RawFilePath.Process.Posix

-- | Start a new sub-process with the given configuration.
startProcess
  :: (StreamType stdin, StreamType stdout, StreamType stderr)
  => ProcessConf stdin stdout stderr
  -> IO (Process stdin stdout stderr)
startProcess = createProcessInternal

-- | Stop a sub-process. For now it simply calls 'terminateProcess' and then
-- 'waitForProcess'.
stopProcess :: Process stdin stdout stderr -> IO ExitCode
stopProcess p = do
  terminateProcess p
  waitForProcess p

-- | Wait (block) for a sub-process to exit and obtain its exit code.
waitForProcess
  :: Process stdin stdout stderr
  -> IO ExitCode
waitForProcess ph = lockWaitpid $ do
  p_ <- modifyProcessHandle ph $ \p_ -> return (p_, p_)
  case p_ of
    ClosedHandle e -> return e
    OpenHandle h -> do
      e <- alloca $ \pret -> do
        -- don't hold the MVar while we call c_waitForProcess...
        throwErrnoIfMinus1Retry_ "waitForProcess" (c_waitForProcess h pret)
        modifyProcessHandle ph $ \p_' ->
          case p_' of
            ClosedHandle e -> return (p_', e)
            OpenExtHandle{} -> return (p_', ExitFailure (-1))
            OpenHandle ph' -> do
              closePHANDLE ph'
              code <- peek pret
              let e =
                    if code == 0
                      then ExitSuccess
                      else ExitFailure (fromIntegral code)
              return (ClosedHandle e, e)
      when delegatingCtlc $
        endDelegateControlC e
      return e
    OpenExtHandle _ _job _iocp ->
      return $ ExitFailure (-1)
 where
  -- If more than one thread calls `waitpid` at a time, `waitpid` will
  -- return the exit code to one of them and (-1) to the rest of them,
  -- causing an exception to be thrown.
  -- Cf. https://github.com/haskell/process/issues/46, and
  -- https://github.com/haskell/process/pull/58 for further discussion
  lockWaitpid m = withMVar (waitpidLock ph) $ \() -> m
  delegatingCtlc = mbDelegateCtlc ph

-- | Terminate a sub-process by sending SIGTERM to it.
terminateProcess :: Process stdin stdout stderr -> IO ()
terminateProcess p = withProcessHandle p $ \case
  ClosedHandle _ -> return ()
  OpenExtHandle{} ->
    error
      "terminateProcess with OpenExtHandle should not happen on POSIX."
  OpenHandle h -> do
    throwErrnoIfMinus1Retry_ "terminateProcess" $ c_terminateProcess h
    return ()

-- does not close the handle, we might want to try terminating it
-- again, or get its exit code.
