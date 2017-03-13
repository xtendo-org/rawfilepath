module System.Process.RawFilePath.Internal where

import Foreign.C
import Foreign.Ptr

import System.Posix.Types
import System.Posix.Internals

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
    -> IO CPid
