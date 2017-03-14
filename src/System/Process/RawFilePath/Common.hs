{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module System.Process.RawFilePath.Common
    ( Process(..)
    , ProcessConf(..)
    , processStdin
    , processStdout
    , processStderr
    , StreamSpec(..)
    , CreatePipe(..)
    , Inherit(..)
    , NoStream(..)
    , UseHandle(..)

    , PHANDLE
    , ProcessHandle(..)
    , ProcessHandle__(..)
    , modifyProcessHandle
    , withProcessHandle
    , fdStdin
    , fdStdout
    , fdStderr
    , mbPipe
    ) where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import GHC.IO.Device
import GHC.IO.Encoding
import GHC.IO.Exception
import GHC.IO.Handle.FD
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types hiding (ClosedHandle)
import GHC.IO.IOMode
import System.IO.Error
import System.Posix.ByteString (RawFilePath)
import System.Posix.Internals
import System.Posix.Types
import qualified GHC.IO.FD as FD

-- Original declarations

-- | The process configuration that is needed for creating new processes.
data ProcessConf stdin stdout stderr = ProcessConf
    { cmdargs :: [ByteString]
    -- ^ Executable & arguments, or shell command
    , cwd :: Maybe RawFilePath
    -- ^ Optional path to the working directory for the new process
    , env :: Maybe [(ByteString, ByteString)]
    -- ^ Optional environment (otherwise inherit from the current process)
    , cfgStdin :: stdin
    -- ^ How to determine stdin
    , cfgStdout :: stdout
    -- ^ How to determine stdout
    , cfgStderr :: stderr
    -- ^ How to determine stderr
    , closeFds :: Bool
    -- ^ Close all file descriptors except stdin, stdout and stderr in the new
    -- process
    , createGroup :: Bool
    -- ^ Create a new process group
    , delegateCtlc :: Bool
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

data CreatePipe = CreatePipe
data Inherit = Inherit
data NoStream = NoStream
data UseHandle = UseHandle Handle

class StreamSpec c where
    mbFd :: FD -> c -> IO FD
    willCreateHandle :: c -> Bool
instance StreamSpec CreatePipe where
    mbFd _ _ = return (-1)
    willCreateHandle _ = True
instance StreamSpec Inherit where
    mbFd std _ = return std
    willCreateHandle _ = False
instance StreamSpec NoStream where
    mbFd _ _ = return (-2)
    willCreateHandle _ = False
instance StreamSpec UseHandle where
    mbFd _std (UseHandle hdl) =
        withHandle "" hdl $ \Handle__{haDevice=dev,..} -> case cast dev of
            Just fd -> do
                -- clear the O_NONBLOCK flag on this FD, if it is set, since
                -- we're exposing it externally (see #3316 of 'process')
                fd' <- FD.setNonBlockingMode fd False
                return (Handle__{haDevice=fd',..}, FD.fdFD fd')
            Nothing -> ioError $ mkIOError illegalOperationErrorType
                "createProcess" (Just hdl) Nothing
                `ioeSetErrorString` "handle is not a file descriptor"
    willCreateHandle _ = False

-- Declarations from the process package (modified)

type PHANDLE = CPid

data ProcessHandle__ = OpenHandle PHANDLE
                     | OpenExtHandle PHANDLE PHANDLE PHANDLE
                     | ClosedHandle ExitCode
data ProcessHandle
  = ProcessHandle { phandle          :: !(MVar ProcessHandle__)
                  , mbDelegateCtlc :: !Bool
                  , waitpidLock      :: !(MVar ())
                  }

modifyProcessHandle
        :: ProcessHandle
        -> (ProcessHandle__ -> IO (ProcessHandle__, a))
        -> IO a
modifyProcessHandle (ProcessHandle m _ _) = modifyMVar m

withProcessHandle :: ProcessHandle -> (ProcessHandle__ -> IO a) -> IO a
withProcessHandle (ProcessHandle m _ _)= withMVar m

fdStdin, fdStdout, fdStderr :: FD
fdStdin  = 0
fdStdout = 1
fdStderr = 2

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
