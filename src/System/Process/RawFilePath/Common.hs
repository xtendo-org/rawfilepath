{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module System.Process.RawFilePath.Common where

import Data.ByteString (ByteString)
import Data.Maybe
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import GHC.IO.Device
import GHC.IO.Encoding
import GHC.IO.Handle.FD
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types hiding (ClosedHandle)
import GHC.IO.IOMode
import System.IO.Error
import System.Posix.ByteString (RawFilePath)
import System.Posix.Internals
import System.Posix.Types
import qualified GHC.IO.FD as FD

-- | The process configuration that is needed for creating new processes.
data ProcessConf stdin stdout stderr = ProcessConf
    { cmdspec      :: CmdSpec
    -- ^ Executable & arguments, or shell command
    , cwd          :: Maybe RawFilePath
    -- ^ Optional path to the working directory for the new process
    , env          :: Maybe [(ByteString, ByteString)]
    -- ^ Optional environment (otherwise inherit from the current process)
    , cfgStdIn       :: stdin
    -- ^ How to determine stdin
    , cfgStdOut      :: stdout
    -- ^ How to determine stdout
    , cfgStdErr      :: stderr
    -- ^ How to determine stderr
    , closeFds    :: Bool
    -- ^ Close all file descriptors except stdin, stdout and stderr in the new
    -- process
    , createGroup :: Bool
    -- ^ Create a new process group
    , delegateCtlc:: Bool
    -- ^ Delegate control-C handling. Use this for interactive console
    -- processes to let them handle control-C themselves (see below for
    -- details).
    , createNewConsole :: Bool
    -- ^ Use the windows CREATE_NEW_CONSOLE flag when creating the process;
    -- does nothing on other platforms.
    --
    -- Default: @False@
    , newSession :: Bool
    -- ^ Use posix setsid to start the new process in a new session; does nothing on other platforms.
    , childGroup :: Maybe GroupID
    -- ^ Use posix setgid to set child process's group id.
    --
    -- Default: @Nothing@
    , childUser :: Maybe UserID
    -- ^ Use posix setuid to set child process's user id.
    --
    -- Default: @Nothing@
    }

data CmdSpec
    = ShellCommand ByteString
    -- ^ A command line to execute using the shell
    | RawCommand RawFilePath [ByteString]
    -- ^ The name of an executable with a list of arguments
    --
    -- The 'RawFilePath' argument names the executable, and is interpreted
    -- according to the platform's standard policy for searching for
    -- executables. The
    -- <http://pubs.opengroup.org/onlinepubs/9699919799/functions/execvp.html execvp(3)>
    -- semantics is used, where if the executable filename does not
    -- contain a slash (@/@) then the @PATH@ environment variable is
    -- searched for the executable.

data Process stdin stdout stderr = Process
    { procStdin :: Maybe Handle
    , procStdout :: Maybe Handle
    , procStderr :: Maybe Handle
    }

processStdin :: Process CreatePipe stdout stderr -> Handle
processStdin Process{..} = fromMaybe err procStdin
  where
    err = error "This can't happen: stdin is CreatePipe but missing"

processStdout :: Process stdin CreatePipe stderr -> Handle
processStdout Process{..} = fromMaybe err procStdout
  where
    err = error "This can't happen: stdout is CreatePipe but missing"

processStderr :: Process stdin stdout CreatePipe -> Handle
processStderr Process{..} = fromMaybe err procStderr
  where
    err = error "This can't happen: stderr is CreatePipe but missing"

startProcess
    :: ProcessConf stdin stdout stderr
    -> IO (Process stdin stdout stderr)
startProcess ProcessConf{..} = do
    _ <- undefined
    return (Process Nothing Nothing Nothing)

data CreatePipe = CreatePipe
data Inherit = Inherit
data NoStream = NoStream
data UseHandle = UseHandle Handle

class StreamSpec c where
    mbFd :: String -> FD -> c -> IO FD
    willCreateHandle :: c -> Bool
instance StreamSpec CreatePipe where
    mbFd _ _ _ = return (-1)
    willCreateHandle _ = True
instance StreamSpec Inherit where
    mbFd _ std _ = return std
    willCreateHandle _ = False
instance StreamSpec NoStream where
    mbFd _ _ _ = return (-2)
    willCreateHandle _ = False
instance StreamSpec UseHandle where
    mbFd fun _std (UseHandle hdl) =
        withHandle fun hdl $ \Handle__{haDevice=dev,..} -> case cast dev of
            Just fd -> do
                -- clear the O_NONBLOCK flag on this FD, if it is set, since
                -- we're exposing it externally (see #3316 of 'process')
                fd' <- FD.setNonBlockingMode fd False
                return (Handle__{haDevice=fd',..}, FD.fdFD fd')
            Nothing -> ioError $ mkIOError illegalOperationErrorType
                "createProcess" (Just hdl) Nothing
                `ioeSetErrorString` "handle is not a file descriptor"
    willCreateHandle _ = False

mbPipe :: StreamSpec c => c -> Ptr FD -> IOMode -> IO (Maybe Handle)
mbPipe streamConf pfd  mode = if willCreateHandle streamConf
    then fmap Just (pfdToHandle pfd mode)
    else return Nothing

pfdToHandle :: Ptr FD -> IOMode -> IO Handle
pfdToHandle pfd mode = do
  fd <- peek pfd
  let filepath = "fd:" ++ show fd
  (fD,fd_type) <- FD.mkFD (fromIntegral fd) mode
                       (Just (Stream,0,0)) -- avoid calling fstat()
                       False {-is_socket-}
                       False {-non-blocking-}
  fD' <- FD.setNonBlockingMode fD True -- see #3316
#if __GLASGOW_HASKELL__ >= 704
  enc <- getLocaleEncoding
#else
  let enc = localeEncoding
#endif
  mkHandleFromFD fD' fd_type filepath mode False {-is_socket-} (Just enc)

