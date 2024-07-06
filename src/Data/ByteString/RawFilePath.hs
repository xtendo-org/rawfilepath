-- |
-- Module      : Data.ByteString.RawFilePath
-- Copyright   : (c) XT 2016
-- License     : Apache 2.0
--
-- Maintainer  : e@xtendo.org
-- Stability   : stable
-- Portability : POSIX
--
-- A drop-in replacement of @Data.ByteString@ from the @bytestring@ package
-- that provides file I/O functions with 'RawFilePath' instead of 'FilePath'.
module Data.ByteString.RawFilePath (
  module Data.ByteString,
  RawFilePath,
  readFile,
  writeFile,
  appendFile,
  withFile,
) where

-- base modules

import Control.Exception (bracket)
-- extra modules

import Data.ByteString hiding (appendFile, readFile, writeFile)
import System.IO (Handle, IOMode (..), hClose)
import System.Posix.ByteString
import Prelude hiding (appendFile, readFile, writeFile)

-- | Read an entire file at the 'RawFilePath' strictly into a 'ByteString'.
readFile :: RawFilePath -> IO ByteString
readFile path = withFile path ReadMode hGetContents

-- | Write a 'ByteString' to a file at the 'RawFilePath'.
writeFile :: RawFilePath -> ByteString -> IO ()
writeFile path content = withFile path WriteMode (`hPut` content)

-- | Append a 'ByteString' to a file at the 'RawFilePath'.
appendFile :: RawFilePath -> ByteString -> IO ()
appendFile path content = withFile path AppendMode (`hPut` content)

-- | Acquire a file handle and perform an I/O action. The file will be closed
-- on exit or when this I/O action throws an exception.
withFile :: RawFilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path ioMode = bracket (open >>= fdToHandle) hClose
 where
#if MIN_VERSION_unix(2,8,0)
    open = case ioMode of
        ReadMode -> openFd path ReadOnly $ defaultFlags Nothing
        WriteMode -> createFile path stdFileMode
        AppendMode -> openFd path WriteOnly $ appendFlags $ Just stdFileMode
        ReadWriteMode -> openFd path ReadWrite $ defaultFlags $ Just stdFileMode
    defaultFlags creat = OpenFileFlags
        { System.Posix.ByteString.append = False
        , creat = creat
        , exclusive = False
        , noctty = True
        , nonBlock = False
        , trunc = False
        , nofollow = False
        , cloexec = False
        , directory = False
        , sync = False
        }
    appendFlags creat = (defaultFlags creat) { System.Posix.ByteString.append = True }
#else
    open = case ioMode of
        ReadMode -> openFd path ReadOnly Nothing defaultFlags
        WriteMode -> createFile path stdFileMode
        AppendMode -> openFd path WriteOnly (Just stdFileMode) appendFlags
        ReadWriteMode -> openFd path ReadWrite (Just stdFileMode) defaultFlags
    defaultFlags = OpenFileFlags
        { System.Posix.ByteString.append = False
        , exclusive = False
        , noctty = True
        , nonBlock = False
        , trunc = False
        }
    appendFlags = defaultFlags { System.Posix.ByteString.append = True }
#endif
