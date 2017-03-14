{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module System.Process.RawFilePath.Posix
    ( mkProcessHandle
    , createProcessInternal
    , withCEnvironment
    , closePHANDLE
    , startDelegateControlC
    , endDelegateControlC
    , stopDelegateControlC
    , c_execvpe
    , pPrPr_disableITimers
    , createPipeInternal
    , createPipeInternalFd
    ) where

-- base modules

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Monoid
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.IO.Exception
import System.IO
import System.IO.Unsafe

-- extra modules

import Data.ByteString (ByteString)
import System.Posix.ByteString.FilePath
import System.Posix.Internals hiding (withFilePath)
import System.Posix.Process.Internals ( pPrPr_disableITimers, c_execvpe )
import System.Posix.Signals as Sig
import System.Posix.Types
import qualified System.Posix.IO as Posix

-- local modules

import System.Process.RawFilePath.Common

#include "processFlags.h"

mkProcessHandle :: PHANDLE -> Bool -> IO ProcessHandle
mkProcessHandle p mbDelegateCtlc = do
  m <- newMVar (OpenHandle p)
  l <- newMVar ()
  return (ProcessHandle m mbDelegateCtlc l)

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE _ = return ()

-- ----------------------------------------------------------------------------
-- Utils

withManyByteString :: [ByteString] -> ([CString] -> IO a) -> IO a
withManyByteString = undefined

withCEnvironment :: [(ByteString, ByteString)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name <> "=" <> val) envir
  in withManyByteString env' (\pEnv -> withArray0 nullPtr pEnv act)

-- -----------------------------------------------------------------------------
-- POSIX runProcess with signal handling in the child

createProcessInternal
    :: (StreamSpec stdin, StreamSpec stdout, StreamSpec stderr)
    => ProcessConf stdin stdout stderr
    -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcessInternal ProcessConf{..}
  = alloca $ \ pfdStdInput  ->
    alloca $ \ pfdStdOutput ->
    alloca $ \ pfdStdError  ->
    alloca $ \ pFailedDoing ->
    maybeWith withCEnvironment env $ \pEnv ->
    maybeWith withFilePath cwd $ \pWorkDir ->
    maybeWith with childGroup $ \pChildGroup ->
    maybeWith with childUser $ \pChildUser ->
    withManyByteString cmdargs $ \cstrs ->
    withArray0 nullPtr cstrs $ \pargs -> do

        fdin  <- mbFd fdStdin  cfgStdin
        fdout <- mbFd fdStdout cfgStdout
        fderr <- mbFd fdStderr cfgStderr

        when delegateCtlc startDelegateControlC

        -- runInteractiveProcess() blocks signals around the fork().
        -- Since blocking/unblocking of signals is a global state
        -- operation, we better ensure mutual exclusion of calls to
        -- runInteractiveProcess().
        proc_handle <- withMVar runInteractiveProcessLock $ \_ ->
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

        when (proc_handle == -1) $ do
            cFailedDoing <- peek pFailedDoing
            failedDoing <- peekCString cFailedDoing
            when delegateCtlc stopDelegateControlC
            -- TODO(XT): avoid String
            throwErrno (show (head cmdargs) ++ ": " ++ failedDoing)

        hndStdInput  <- mbPipe cfgStdin  pfdStdInput  WriteMode
        hndStdOutput <- mbPipe cfgStdout pfdStdOutput ReadMode
        hndStdError  <- mbPipe cfgStderr pfdStdError  ReadMode

        ph <- mkProcessHandle proc_handle delegateCtlc
        return (hndStdInput, hndStdOutput, hndStdError, ph)

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
    ::  Ptr CString
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

createPipeInternal :: IO (Handle, Handle)
createPipeInternal = do
    (readfd, writefd) <- Posix.createPipe
    readh <- Posix.fdToHandle readfd
    writeh <- Posix.fdToHandle writefd
    return (readh, writeh)

createPipeInternalFd :: IO (FD, FD)
createPipeInternalFd = do
   (Fd readfd, Fd writefd) <- Posix.createPipe
   return (readfd, writefd)
