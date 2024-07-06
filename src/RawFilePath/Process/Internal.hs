{-# LANGUAGE InterruptibleFFI #-}

module RawFilePath.Process.Internal (
  c_terminateProcess,
  c_getProcessExitCode,
  c_waitForProcess,
) where

import Foreign
import Foreign.C
import RawFilePath.Process.Common
import System.Posix.Types (CPid (..))

foreign import ccall unsafe "terminateProcess"
  c_terminateProcess
    :: PHANDLE
    -> IO CInt

foreign import ccall unsafe "getProcessExitCode"
  c_getProcessExitCode
    :: PHANDLE
    -> Ptr CInt
    -> IO CInt

foreign import ccall interruptible "waitForProcess" -- NB. safe - can block
  c_waitForProcess
    :: PHANDLE
    -> Ptr CInt
    -> IO CInt
