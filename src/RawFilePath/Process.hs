-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  RawFilePath.Process
-- Copyright   :  (C) XT et al. 2017 - 2024
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
-- unnecessary runtime errors when obtaining t'System.IO.Handle's. This is
-- inspired by the @typed-process@ package which is awesome, although this
-- module is much simpler; it doesn't introduce any new requirement of language
-- extension or library package (for the sake of portability).
--
-- t'System.IO.Handle' (accessible with 'processStdin', 'processStdout', and
-- 'processStderr') is what you can use to interact with the sub-process. For
-- example, use 'Data.ByteString.hGetContents' from "Data.ByteString" to read
-- from a t'System.IO.Handle' as a 'ByteString'.
--
-- == Fast and brief example
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
-- == Verbose explanation of the example
--
-- We launch @sed@ as a child process. As we know, it is a regular expression
-- search and replacement tool. In the example, @sed@ is a simple Unix pipe
-- utility: Take some text from @stdin@ and output the processed text to
-- @stdout@.
--
-- In @sed@ regex,
--
-- @
-- \\\>
-- @
--
-- means "the end of the word." So,
--
-- @
-- "s\/\\\\\>\/!\/g"
-- @
--
-- means "substitute all ends of the words with an exclamation mark." Then, we
-- feed some text to its @stdin@, close @stdin@ (to send EOF to @sed@'s
-- @stdin@), and read what it wrote to @stdout@.
--
-- The interesting part is 'proc'. It is a simple function that takes a
-- command and its arguments and returns a 'ProcessConf' which defines the
-- properties of the child process you want to create. You can use
-- functions like 'setStdin' or 'setStdout' to change those properties.
--
-- The advantage of this interface is type safety. Take @stdout@ for example.
-- There are four options: 'Inherit', 'UseHandle', 'CreatePipe', and
-- 'NoStream'. If you want to read @stdout@ of the child process, you must set
-- it to @CreatePipe@. With the @process@ package, this is done by giving a
-- proper argument to @createProcess@. The trouble is, regardless of the
-- argument, @createProcess@ returns 'Maybe' t'System.IO.Handle' as @stdout@.
-- You may or may not get a t'System.IO.Handle'.
--
-- This is not what we want with Haskell. We want to ensure that
--
--  1.  We use 'CreatePipe' and certainly get the @stdout@ t'System.IO.Handle'
--      (without having to write unncessary handling for the 'Nothing' case that
--      never happens).
--  2.  If we don't use 'CreatePipe' but still request the @stdout@
--      t'System.IO.Handle', it is an error, detected __compile-time__.
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
-- you can later use `processStdout` to get the t'System.IO.Handle'. If you
-- don't put the @\`setStdout\` CreatePipe@ part or set it to something other
-- than @CreatePipe@, it will be a compile-time error to use 'processStdout' on
-- this process object.
--
-- In short, it makes the correct code easy and the wrong code impossible.
-- This approach was inspired by the @typed-process@ package. Then why not
-- just @typed-process@? @rawfilepath@ offers
--
-- 1. 'RawFilePath'! (instead of 'FilePath' which is 'String'...)
-- 2. A lot less dependency (only three packages)
-- 3. A lot more portability (doesn't require any language extension).
--
-- Enjoy.
module RawFilePath.Process (
  RawFilePath,

  -- ** Configuring process
  -- $configuring
  ProcessConf,
  proc,

  -- *** Configuring process standard streams
  StreamType,
  CreatePipe (..),
  Inherit (..),
  NoStream (..),
  UseHandle (..),
  setStdin,
  setStdout,
  setStderr,

  -- ** Running process
  Process,
  startProcess,

  -- ** Obtaining process streams
  -- $obtaining
  processStdin,
  processStdout,
  processStderr,

  -- ** Process completion
  stopProcess,
  terminateProcess,
  waitForProcess,

  -- ** Untyped process
  -- $untyped
  UnknownStream,

  -- *** Functions to untype process streams
  untypeProcess,
  untypeProcessStdin,
  untypeProcessStdout,
  untypeProcessStderr,

  -- *** Functions to obtain @Maybe Handle@ from @UnknownStream@
  processStdinUnknown,
  processStdoutUnknown,
  processStderrUnknown,

  -- ** Utility functions
  -- $utility
  callProcess,
  readProcessWithExitCode,
) where

import RawFilePath.Import
-- local modules

import RawFilePath.Process.Basic
import RawFilePath.Process.Common
import RawFilePath.Process.Utility

-- $configuring
--
-- Configuration of how a new sub-process will be launched.

-- $obtaining
--
-- As the type signature suggests, these functions only work on processes whose
-- stream in configured to 'CreatePipe'. This is the type-safe way of obtaining
-- t'System.IO.Handle's instead of returning 'Maybe' t'System.IO.Handle's like
-- the @process@ package does.

-- $untyped
--
-- @since 1.1.1
--
-- Type safety is awesome, and having types like @Process NoStream
-- CreatePipe Inherit@ is the whole point of this module.
--
-- However, __after__ we've dealt with many sub-processes and their stream
-- t'System.IO.Handle's, we may have
--
--  * 'Process' 'Inherit' 'CreatePipe' 'Inherit'
--
--  * 'Process' 'CreatePipe' 'Inherit' 'Inherit'
--
--  * 'Process' 'CreatePipe' 'CreatePipe' 'CreatePipe'
--
--  * 'Process' 'NoStream' 'Inherit' 'Inherit'
--
--  * 'Process' 'NoStream' 'CreatePipe' 'Inherit'
--
--  * 'Process' 'Inherit' 'CreatePipe' 'CreatePipe'
--
--  * ...
--
-- You get the point. It gets out of hand! There are \( 4^3 = 64 \)
-- combinations and they are all "different process types." You can't put them
-- in the same basket. There are realistic reasons you'd want this:
--
--  * To keep track of many sub-processes you create, and later properly clean
--    them up.
--
--  * To have a group of /partially typed/ sub-processes: For example, if you
--    have one @Process NoStream 'CreatePipe' Inherit@ and one @Process
--    CreatePipe 'CreatePipe' CreatePipe@, both are guaranteed to have
--    __stdout__. You'd want to put them into a list, and later loop over them
--    to collect the stdout t'System.IO.Handle's.
--
-- (Maybe you can use extensions like @ExistentialQuantification@ for this
-- but... It's like you're trying to safeguard your house by installing iron
-- walls in front of the gate, then bringing in a tunnel boring machine to
-- construct an underground passage. Also, we advertised rawfilepath with
-- "minimal dependencies" and "high portability.")
--
-- This is why rawfilepath now provides 'UnknownStream' and its related
-- functions. So,
--
-- @
-- 'Process' 'UnknownStream' 'UnknownStream' 'UnknownStream'
-- @
--
-- ... is much like the traditional, un-typed process. You can get a 'Maybe'
-- t'System.IO.Handle' for an 'UnknownStream'. This is useful when you
--
--  1.  ... Are done with any standard stream I/O
--  2.  ... No longer need the compile-time guarantee of getting (or being
--      prevented to get) t'System.IO.Handle's.
--  3.  ... But still need the 'Process'

-- $utility
--
-- These are utility functions; they can be implemented with the primary
-- functions above. They are provided for convenience.
