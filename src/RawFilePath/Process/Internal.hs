{-# LANGUAGE InterruptibleFFI #-}

module RawFilePath.Process.Internal
    ( c_terminateProcess
    , c_getProcessExitCode
    , c_waitForProcess
    ) where

import Foreign
import Foreign.C

import System.Posix.Types (CPid (..))

import RawFilePath.Process.Common

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
