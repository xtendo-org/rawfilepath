-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  RawFilePath
-- Copyright   :  (C) XT et al. 2017
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  e@xtendo.org
-- Stability   :  stable
-- Portability :  POSIX
--
-- Welcome to @RawFilePath@, a small part of the Haskell community's effort to
-- purge 'String' for the Greater Good.
--
-- With this package, you can interact with the Unix system without the file
-- path encoding issue or the 'String' ↔ 'ByteString' conversion overhead.
--
-- == Rationale
--
-- Traditional `String` is notorious:
--
-- * 24 bytes (three words) required for one character (the List constructor, the actual Char value, and the pointer to the next List constructor). 24x memory consumption.
-- * Heap fragmentation causing malloc/free overhead
-- * A lot of pointer chasing for reading, devastating the cache hit rate
-- * A lot of pointer chasing plus a lot of heap object allocation for manipulation (appending, slicing, etc.)
-- - Completely unnecessary but mandatory conversions and memory allocation when the data is sent to or received from the outside world
--
-- `FilePath` is a type synonym of `String`. This is a bigger problem than what `String` already has, because it's not just a performance issue anymore; it's a correctness issue as there is no encoding information.
--
-- A syscall would give you (or expect from you) a series of bytes, but `String` is a series of characters. But how do you know the system's encoding? NTFS is UTF-16, and FAT32 uses the OEM character set. On Linux, there is no filesystem-level encoding. Would Haskell somehow magically figure out the system's encoding information and encode/decode accordingly? Well, there is no magic. `FilePath` has completely no guarantee of correct behavior at all, especially when there are non-ASCII letters.
--
-- With this library, you use 'RawFilePath' which is a sequence of bytes (instead of characters). You have the full control of decoding from (or encoding to) these bytes. This lets you do the job properly.
--
-- == Usage
--
-- This is the top-level module that re-exports the sub-modules. Therefore,
-- you can
--
-- @
-- import RawFilePath
-- @
--
-- to import all functions. For documentation, see:
--
-- * "RawFilePath.Directory"
-- * "RawFilePath.Process"
--
-- For process-related functions, see "RawFilePath.Process" for a brief
-- introduction and an example code.
module RawFilePath (
  module RawFilePath.Directory,
  module RawFilePath.Process,
  RawFilePath,
) where

-- local modules

import RawFilePath.Directory hiding (RawFilePath)
import RawFilePath.Import
import RawFilePath.Process hiding (RawFilePath)
