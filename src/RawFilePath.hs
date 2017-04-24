-----------------------------------------------------------------------------
-- |
-- Module      :  RawFilePath.Process
-- Copyright   :  (C) XT et al. 2017
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  e@xtendo.org
-- Stability   :  experimental
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
-- 'String' has another problematic nature to serve as a file path data type: Encoding blindness. All functions that return 'FilePath' would actually take a series of bytes returned by a syscall and somehow magically "decode" it into a `String` which is surprising because no encoding information was given. Of course there is no magic and it's an abject fail. 'FilePath' just wouldn't work.
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
-- to import all functions.
--
-----------------------------------------------------------------------------

module RawFilePath
    ( module RawFilePath.Directory
    , module RawFilePath.Process
    , RawFilePath
    ) where

import RawFilePath.Import

-- local modules

import RawFilePath.Directory hiding (RawFilePath)
import RawFilePath.Process hiding (RawFilePath)
