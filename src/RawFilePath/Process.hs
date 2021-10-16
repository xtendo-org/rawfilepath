-----------------------------------------------------------------------------
-- |
-- Module      :  RawFilePath.Process
-- Copyright   :  (C) XT et al. 2017
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  e@xtendo.org
-- Stability   :  stable
-- Portability :  POSIX
--
-- Welcome to @RawFilePath.Process@, a small part of the Haskell community's
-- effort to replace 'String' for the Greater Good.
--
-- With this module, you can create (and interact with) sub-processes without
-- the encoding problem of 'String'. The command and its arguments, all
-- 'ByteString's, never get converted from/to 'String' internally on its way
-- to the actual syscall. It also avoids the time/space waste of 'String'.
--
-- The interface, unlike the original @process@ package, uses types to prevent
-- unnecessary runtime errors when obtaining 'Handle's. This is inspired by
-- the @typed-process@ package which is awesome, although this module is much
-- simpler; it doesn't introduce any new requirement of language extension or
-- library package (for the sake of portability).
--
-- 'Handle' (accessible with 'processStdin', 'processStdout', and
-- 'processStderr') is what you can use to interact with the sub-process. For
-- example, use 'Data.ByteString.hGetContents' from "Data.ByteString" to read
-- from a 'Handle' as a 'ByteString'.
--
-- == Fast and Brief Example
--
-- If you have experience with Unix pipes, this example should be pretty
-- straightforward. In fact it is so simple that you don't need any type
-- theory or PL knowledge. It demonstrates how you can create a child process
-- and interact with it.
--
-- @
-- {-\# language OverloadedStrings \#-}
--
-- import RawFilePath.Process
-- import System.IO
-- import qualified Data.ByteString as B
--
--
-- main :: IO ()
-- main = do
--   p \<- 'startProcess' $ 'proc' "sed" ["-e", "s\/\\\\\>\/!\/g"]
--     \`'setStdin'\` 'CreatePipe'
--     \`'setStdout'\` 'CreatePipe'
--   B.hPut ('processStdin' p) "Lorem ipsum dolor sit amet"
--   hClose ('processStdin' p)
--   result <- B.hGetContents ('processStdout' p)
--   print result
--   -- "Lorem! ipsum! dolor! sit! amet!"
-- @
--
-- That's it! You can totally skip the verbose explanation below.
--
-- == Verbose Explanation of the Example
--
-- We launch @sed@ as a child process. As we know, it is a regular expression
-- search and replacement tool. In the example, @sed@ is a simple Unix pipe
-- utility: Take some text from @stdin@ and output the processed text to
-- @stdout@.
--
-- In @sed@ regex, @\\\>@ means "the end of the word." So, @"s\/\\\\\>\/!\/g"@
-- means "substitute all ends of the words with an exclamation mark." Then, we
-- feed some text to its @stdin@, close @stdin@ (to send EOF to @sed@ EOF),
-- and read what it said to @stdout@.
--
-- The interesting part is 'proc'. It is a simple function that takes a
-- command and its arguments and returns a 'ProcessConf' which defines the
-- properties of the child process you want to create. You can use
-- functions like 'setStdin' or 'setStdout' to change those properties.
--
-- The advantage of this interface is type safety. Take @stdout@ for example.
-- There are four options: @Inherit@, @UseHandle@, @CreatePipe@, and
-- @NoStream@. If you want to read @stdout@ of the child process, you must set
-- it to @CreatePipe@. With the @process@ package, this is done by giving a
-- proper argument to @createProcess@. The trouble is, regardless of the
-- argument, @createProcess@ returns 'Maybe' 'Handle' as @stdout@. You may or
-- may not get a 'Handle'.
--
-- This is not what we want with Haskell. We want to ensure that (1) we use
-- 'CreatePipe' and certainly get the @stdout@ 'Handle' without the fear of
-- 'Nothing', and (2) if we don't use 'CreatePipe' but still request the
-- @stdout@ 'Handle', it is an error, detected at compile time.
--
-- So that's what @RawFilePath.Process@ does. In the above example, we use
-- functions like 'setStdout'. Later, you use the 'processStdout' family of
-- functions to get the process's standard stream handles. This requires that
-- the process was created with 'CreatePipe' appropriately set for that
-- stream.
--
-- It sounds all complicated, but all you really need to do is as simple as:
--
-- @
-- 'startProcess' $ 'proc' \"...\" [...] \`'setStdout'\` 'CreatePipe'
-- @
--
-- ... If you want to create a new pipe for the child process's @stdin@. Then
-- you can later use `processStdout` to get the 'Handle'. If you don't put the
-- @\`setStdout\` CreatePipe@ part or set it to something other than
-- @CreatePipe@, it will be a compile-time error to use 'processStdout' on
-- this process object.
--
-- In short, it makes the correct code easy and the wrong code impossible.
-- This approach was inspired by the @typed-process@ package. Then why not
-- just @typed-process@? @rawfilepath@ offers
--
-- 1. RawFilePath!
-- 2. A lot less dependency (only three packages)
-- 3. A lot more portability (doesn't require any language extension).
--
-- Enjoy.
--
-----------------------------------------------------------------------------


module RawFilePath.Process
    ( RawFilePath
    -- ** Configuring process
    -- $configuring
    , ProcessConf
    , proc

    -- *** Configuring process standard streams
    , StreamType
    , CreatePipe(..)
    , Inherit(..)
    , NoStream(..)
    , UseHandle(..)
    , setStdin
    , setStdout
    , setStderr

    -- ** Running process
    , Process
    , startProcess

    -- ** Obtaining process streams
    -- $obtaining
    , processStdin
    , processStdout
    , processStderr

    -- ** Process completion
    , stopProcess
    , terminateProcess
    , waitForProcess

    -- ** Utility functions
    -- $utility
    , callProcess
    , readProcessWithExitCode

    ) where

import RawFilePath.Import

-- local modules

import RawFilePath.Process.Basic
import RawFilePath.Process.Common
import RawFilePath.Process.Utility

-- $configuring
--
-- Configuration of how a new sub-process will be launched.
--
-- $obtaining
--
-- As the type signature suggests, these functions only work on processes
-- whose stream in configured to 'CreatePipe'. This is the type-safe way of
-- obtaining 'Handle's instead of returning 'Maybe' 'Handle's like the
-- @process@ package does.
--
-- $utility
--
-- These are utility functions; they can be implemented with the primary
-- functions above. They are provided for convenience.
