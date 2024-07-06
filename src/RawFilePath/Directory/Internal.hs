module RawFilePath.Directory.Internal where

import RawFilePath.Import
import qualified System.Posix.ByteString as U

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = ioeSetLocation e newLoc
 where
  newLoc = loc <> if null oldLoc then "" else ":" <> oldLoc
  oldLoc = ioeGetLocation e

data FileType
  = File
  | SymbolicLink
  | Directory
  | DirectoryLink
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

fileTypeFromMetadata :: U.FileStatus -> FileType
fileTypeFromMetadata stat
  | U.isSymbolicLink stat = SymbolicLink
  | U.isDirectory stat = Directory
  | otherwise = File
