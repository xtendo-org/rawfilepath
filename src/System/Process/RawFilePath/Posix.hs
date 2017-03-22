module System.Process.RawFilePath.Posix
    ( createProcessInternal
    , withCEnvironment
    , closePHANDLE
    , startDelegateControlC
    , endDelegateControlC
    , stopDelegateControlC
    , c_execvpe
    , pPrPr_disableITimers
    , createPipe
    , createPipeInternalFd
    ) where

import RawFilePath.Import

-- extra modules

import Data.ByteString.Internal (ByteString(..), memcpy)
import System.Posix.ByteString.FilePath (withFilePath)
import System.Posix.Internals hiding (withFilePath)
import System.Posix.Process.Internals ( pPrPr_disableITimers, c_execvpe )
import System.Posix.Signals
import qualified System.Posix.Signals as Sig
import qualified System.Posix.IO as Posix

-- local modules

import System.Process.RawFilePath.Common

#include "processFlags.c"

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE _ = return ()

-- ----------------------------------------------------------------------------
-- Utils

withManyByteString :: [ByteString] -> (Ptr CString -> IO a) -> IO a
withManyByteString bs action =
  allocaBytes wholeLength $ \ buf ->
  allocaBytes ptrLength $ \ cs -> do
    copyByteStrings bs buf cs
    action (castPtr cs)
  where
    ptrLength = (length bs + 1) * sizeOf (undefined :: Ptr CString)
    wholeLength = sum (map (\ (PS _ _ l) -> l + 1) bs)

copyByteStrings :: [ByteString] -> Ptr Word8 -> Ptr (Ptr Word8) -> IO ()
copyByteStrings [] _ cs = poke cs nullPtr
copyByteStrings (PS fp o l : xs) buf cs = withForeignPtr fp $ \ p -> do
    memcpy buf (p `plusPtr` o) (fromIntegral l)
    pokeByteOff buf l (0 :: Word8)
    poke cs (buf :: Ptr Word8)
    copyByteStrings xs (buf `plusPtr` (l + 1))
        (cs `plusPtr` sizeOf (undefined :: Ptr CString))

withCEnvironment :: [(ByteString, ByteString)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name <> "=" <> val) envir
  in withManyByteString env' act

-- -----------------------------------------------------------------------------
-- POSIX runProcess with signal handling in the child

createProcessInternal
    :: (StreamType stdin, StreamType stdout, StreamType stderr)
    => ProcessConf stdin stdout stderr
    -> IO (Process stdin stdout stderr)
createProcessInternal ProcessConf{..}
  = alloca $ \ pfdStdInput  ->
    alloca $ \ pfdStdOutput ->
    alloca $ \ pfdStdError  ->
    alloca $ \ pFailedDoing ->
    maybeWith withCEnvironment env $ \pEnv ->
    maybeWith withFilePath cwd $ \pWorkDir ->
    maybeWith with childGroup $ \pChildGroup ->
    maybeWith with childUser $ \pChildUser ->
    withManyByteString cmdargs $ \pargs -> do

        fdin  <- mbFd fdStdin  cfgStdin
        fdout <- mbFd fdStdout cfgStdout
        fderr <- mbFd fdStderr cfgStderr

        when delegateCtlc startDelegateControlC

        -- runInteractiveProcess() blocks signals around the fork().
        -- Since blocking/unblocking of signals is a global state
        -- operation, we better ensure mutual exclusion of calls to
        -- runInteractiveProcess().
        procHandle <- withMVar runInteractiveProcessLock $ \_ ->
          c_runInteractiveProcess pargs pWorkDir pEnv
            fdin fdout fderr
            pfdStdInput pfdStdOutput pfdStdError
            pChildGroup pChildUser
            (if delegateCtlc then 1 else 0)
            ((if closeFds then RUN_PROCESS_IN_CLOSE_FDS else 0)
            .|.(if createGroup then RUN_PROCESS_IN_NEW_GROUP else 0)
            .|.(if createNewConsole then RUN_PROCESS_NEW_CONSOLE else 0)
            .|.(if newSession then RUN_PROCESS_NEW_SESSION else 0))
            pFailedDoing

        when (procHandle == -1) $ do
            cFailedDoing <- peek pFailedDoing
            failedDoing <- peekCString cFailedDoing
            when delegateCtlc stopDelegateControlC
            -- TODO(XT): avoid String
            throwErrno (show (head cmdargs) ++ ": " ++ failedDoing)

        hIn  <- mbPipe cfgStdin  pfdStdInput  WriteMode
        hOut <- mbPipe cfgStdout pfdStdOutput ReadMode
        hErr <- mbPipe cfgStderr pfdStdError  ReadMode

        mvarProcHandle <- newMVar (OpenHandle procHandle)
        lock <- newMVar ()
        return (Process hIn hOut hErr mvarProcHandle delegateCtlc lock)

{-# NOINLINE runInteractiveProcessLock #-}
runInteractiveProcessLock :: MVar ()
runInteractiveProcessLock = unsafePerformIO $ newMVar ()

-- ----------------------------------------------------------------------------
-- Delegated control-C handling on Unix

-- See ticket https://ghc.haskell.org/trac/ghc/ticket/2301
-- and http://www.cons.org/cracauer/sigint.html
--
-- While running an interactive console process like ghci or a shell, we want
-- to let that process handle Ctl-C keyboard interrupts how it sees fit.
-- So that means we need to ignore the SIGINT/SIGQUIT Unix signals while we're
-- running such programs. And then if/when they do terminate, we need to check
-- if they terminated due to SIGINT/SIGQUIT and if so then we behave as if we
-- got the Ctl-C then, by throwing the UserInterrupt exception.
--
-- If we run multiple programs like this concurrently then we have to be
-- careful to avoid messing up the signal handlers. We keep a count and only
-- restore when the last one has finished.

{-# NOINLINE runInteractiveProcessDelegateCtlc #-}
runInteractiveProcessDelegateCtlc :: MVar (Maybe (Int, Sig.Handler, Sig.Handler))
runInteractiveProcessDelegateCtlc = unsafePerformIO $ newMVar Nothing

startDelegateControlC :: IO ()
startDelegateControlC =
    modifyMVar_ runInteractiveProcessDelegateCtlc $ \ case
        Nothing -> do
          -- We're going to ignore ^C in the parent while there are any
          -- processes using ^C delegation.
          --
          -- If another thread runs another process without using
          -- delegation while we're doing this then it will inherit the
          -- ignore ^C status.
          old_int  <- installHandler sigINT  Ignore Nothing
          old_quit <- installHandler sigQUIT Ignore Nothing
          return (Just (1, old_int, old_quit))

        Just (count, old_int, old_quit) -> do
          -- If we're already doing it, just increment the count
          let !count' = count + 1
          return (Just (count', old_int, old_quit))

stopDelegateControlC :: IO ()
stopDelegateControlC =
    modifyMVar_ runInteractiveProcessDelegateCtlc $ \ case
        Just (1, old_int, old_quit) -> do
          -- Last process, so restore the old signal handlers
          _ <- installHandler sigINT  old_int  Nothing
          _ <- installHandler sigQUIT old_quit Nothing
          return Nothing

        Just (count, old_int, old_quit) -> do
          -- Not the last, just decrement the count
          let !count' = count - 1
          return (Just (count', old_int, old_quit))

        Nothing -> return Nothing -- should be impossible

endDelegateControlC :: ExitCode -> IO ()
endDelegateControlC exitCode = do
    stopDelegateControlC

    -- And if the process did die due to SIGINT or SIGQUIT then
    -- we throw our equivalent exception here (synchronously).
    --
    -- An alternative design would be to throw to the main thread, as the
    -- normal signal handler does. But since we can be sync here, we do so.
    -- It allows the code locally to catch it and do something.
    case exitCode of
      ExitFailure n | isSigIntQuit n -> throwIO UserInterrupt
      _                              -> return ()
  where
    isSigIntQuit n = sig == sigINT || sig == sigQUIT
      where
        sig = fromIntegral (-n)

foreign import ccall unsafe "runInteractiveProcess"
  c_runInteractiveProcess
    :: Ptr CString
    -> CString
    -> Ptr CString
    -> FD
    -> FD
    -> FD
    -> Ptr FD
    -> Ptr FD
    -> Ptr FD
    -> Ptr CGid
    -> Ptr CUid
    -> CInt                         -- reset child's SIGINT & SIGQUIT handlers
    -> CInt                         -- flags
    -> Ptr CString
    -> IO PHANDLE

createPipe :: IO (Handle, Handle)
createPipe = do
    (readfd, writefd) <- Posix.createPipe
    readh <- Posix.fdToHandle readfd
    writeh <- Posix.fdToHandle writefd
    return (readh, writeh)

createPipeInternalFd :: IO (FD, FD)
createPipeInternalFd = do
   (Fd readfd, Fd writefd) <- Posix.createPipe
   return (readfd, writefd)
