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
-- Welcome to @RawFilePath.Process@, a small part of the Haskell
-- community's effort to purge 'String' for the Greater Good.
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
-- == Example
--
-- @
-- {-\# language OverloadedStrings \#-}
--
-- import RawFilePath.Process
-- import qualified Data.ByteString as B
--
-- main :: IO ()
-- main = do
--     p <- 'startProcess' $ 'proc' "echo" ["hello"]
--         \`setStdout\` 'CreatePipe'
--     result <- B.hGetContents ('processStdout' p)
--     _ <- 'waitForProcess' p
--
--     print (result == "hello\\n")
-- @
--
-----------------------------------------------------------------------------


module RawFilePath.Process
    ( RawFilePath
    -- ** Configuring process
    -- $configuring
    , ProcessConf
    , proc
    , proc'

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
