-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.RawFilePath
-- Copyright   :  (C) XT et al. 2017
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  e@xtendo.org
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Welcome to @System.Process.RawFilePath@, a small part of the glorious
-- crusade of the Haskell community to purge 'String' for the Greater Good.
--
-- With this module, you can create (and interact with) sub-processes without
-- the encoding problem of 'String'. The command and its arguments, all
-- 'ByteString's, never get converted from/to 'String' internally on its way
-- to the actual syscall. It also avoids the time/space waste of 'String'.
--
-- The interface, unlike the original @process@ package, uses types to prevent
-- unnecessary runtime errors when obtaining 'Handle's. This is inspired by
-- the @typed-process@ package which is awesome, although this module is much
-- simpler; it doesn't introduce any new requirement of language extension or
-- library package (for the sake of portability).
--
-- 'Handle' (accessible with 'processStdin', 'processStdout', and
-- 'processStderr') is what you can use to interact with the sub-process. For
-- example, use 'hGetContents' from "Data.ByteString" to read from a 'Handle'
-- as a 'ByteString'.
--
-- == Example
--
-- This is the type of what you get with 'startProcess'. You can stop it with
-- 'stopProcess', or wait (block) for it to exit with 'waitForProcess'.
--
-----------------------------------------------------------------------------


module System.Process.RawFilePath
    (
    -- ** Example
    --
    -- | blah blah blah
    --
    -- $example
    --
    -- ** Configuring process
    -- $configuring
      ProcessConf
    , proc

    -- *** Configuring process standard streams
    , StreamType
    , CreatePipe(..)
    , Inherit(..)
    , NoStream(..)
    , UseHandle(..)
    , setStdin
    , setStdout
    , setStderr

    -- ** Running process
    , Process
    , startProcess

    -- ** Obtaining process streams
    , processStdin
    , processStdout
    , processStderr

    -- ** Process completion
    , stopProcess
    , terminateProcess
    , waitForProcess

    ) where

-- base modules

import RawFilePath.Import hiding (ClosedHandle)

-- extra modules

import Data.ByteString (hGetContents) -- imported for documentation

-- local modules

import System.Process.RawFilePath.Common
import System.Process.RawFilePath.Internal
import System.Process.RawFilePath.Posix

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
  p_ <- modifyProcessHandle ph $ \ p_ -> return (p_,p_)
  case p_ of
    ClosedHandle e -> return e
    OpenHandle h  -> do
        e <- alloca $ \ pret -> do
          -- don't hold the MVar while we call c_waitForProcess...
          throwErrnoIfMinus1Retry_ "waitForProcess" (c_waitForProcess h pret)
          modifyProcessHandle ph $ \ p_' ->
            case p_' of
              ClosedHandle e  -> return (p_', e)
              OpenExtHandle{} -> return (p_', ExitFailure (-1))
              OpenHandle ph'  -> do
                closePHANDLE ph'
                code <- peek pret
                let e = if code == 0
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
    lockWaitpid m = withMVar (waitpidLock ph) $ \ () -> m
    delegatingCtlc = mbDelegateCtlc ph

-- | Terminate a sub-process by sending SIGTERM to it.
terminateProcess :: Process stdin stdout stderr -> IO ()
terminateProcess p = withProcessHandle p $ \ case
    ClosedHandle  _ -> return ()
    OpenExtHandle{} -> error
        "terminateProcess with OpenExtHandle should not happen on POSIX."
    OpenHandle    h -> do
        throwErrnoIfMinus1Retry_ "terminateProcess" $ c_terminateProcess h
        return ()
        -- does not close the handle, we might want to try terminating it
        -- again, or get its exit code.


-- $example
--
-- | blah blah blah
--
-- $configuring
--
-- Configuration of how a new sub-process will be launched.
