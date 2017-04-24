module RawFilePath.Process.Utility
    ( callProcess
    , readProcessWithExitCode
    ) where

-- base modules

import RawFilePath.Import

-- extra modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as B

-- local modules

import RawFilePath.Process.Common
import RawFilePath.Process.Basic

-- | Create a new process with the given configuration, and wait for it to
-- finish.
callProcess :: ProcessConf stdin stdout stderr -> IO ExitCode
callProcess conf = start >>= waitForProcess
  where
    start = startProcess conf
        { cfgStdin = NoStream
        , cfgStdout = NoStream
        , cfgStderr = NoStream
        }

-- | Fork an external process, read its standard output and standard error
-- strictly, blocking until the process terminates, and return them with the
-- process exit code.
readProcessWithExitCode
    :: ProcessConf stdin stdout stderr
    -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode conf = do
    process <- startProcess conf
        { cfgStdin = NoStream
        , cfgStdout = CreatePipe
        , cfgStderr = CreatePipe
        }
    stdoutB <- hGetAll (processStdout process)
    stderrB <- hGetAll (processStderr process)
    exitCode <- waitForProcess process
    return (exitCode, stdoutB, stderrB)

-- utility functions

-- Read from Handle until IOError
hGetAll :: Handle -> IO ByteString
hGetAll h = LB.toStrict . B.toLazyByteString <$> hGetAll' mempty h
  where
    hGetAll' acc h' = tryIOError (B.hGetContents h) >>= \ case
        Left _ -> return acc
        Right b -> hGetAll' (acc <> B.byteString b) h'
