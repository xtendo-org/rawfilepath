-----------------------------------------------------------------------------
-- |
-- Module      :  RawFilePath.Directory
-- Copyright   :  (C) 2004 The University of Glasgow. (C) 2017 XT et al.
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  e@xtendo.org
-- Stability   :  experimental
-- Portability :  POSIX
--
-- This is the module for the 'RawFilePath' version of functions in the
-- @directory@ package.
--
-----------------------------------------------------------------------------

module RawFilePath.Directory
    ( RawFilePath
    -- ** Nondestructive (read-only)
    , doesPathExist
    , doesFileExist
    , doesDirectoryExist
    , getHomeDirectory
    , getTemporaryDirectory
    , listDirectory
    , getDirectoryFiles
    , getDirectoryFilesRecursive
    -- ** Destructive
    , createDirectory
    , createDirectoryIfMissing
    , removeFile
    , tryRemoveFile
    , removeDirectory
    , removeDirectoryRecursive
    ) where

import RawFilePath.Import

-- extra modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified System.Posix.ByteString as U -- U for Unix

-- local modules

import RawFilePath.Directory.Internal

-- | Test whether the given path points to an existing filesystem object.  If
-- the user lacks necessary permissions to search the parent directories, this
-- function may return false even if the file does actually exist.
doesPathExist :: RawFilePath -> IO Bool
doesPathExist path = (True <$ U.getFileStatus path) `catchIOError`
      const (return False)

-- | Return 'True' if the argument file exists and is either a directory or a
-- symbolic link to a directory, and 'False' otherwise.
doesDirectoryExist :: RawFilePath -> IO Bool
doesDirectoryExist path = pathIsDirectory path `catchIOError`
    const (return False)

-- | Return 'True' if the argument file exists and is not a directory, and
-- 'False' otherwise.
doesFileExist :: RawFilePath -> IO Bool
doesFileExist path = (not <$> pathIsDirectory path) `catchIOError`
    const (return False)

-- | Returns the current user's home directory. More specifically, the value
-- of the @HOME@ environment variable.
--
-- The directory returned is expected to be writable by the current user, but
-- note that it isn't generally considered good practice to store
-- application-specific data here; use 'getXdgDirectory' or
-- 'getAppUserDataDirectory' instead.
--
-- The operation may fail with:
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of home directory.
--
-- * 'isDoesNotExistError'
-- The home directory for the current user does not exist, or
-- cannot be found.
getHomeDirectory :: IO (Maybe RawFilePath)
getHomeDirectory = U.getEnv "HOME"

-- | Return the current directory for temporary files.  It first returns the
-- value of the @TMPDIR@ environment variable or \"\/tmp\" if the variable
-- isn\'t defined.
getTemporaryDirectory :: IO ByteString
getTemporaryDirectory = fromMaybe "/tmp" <$> U.getEnv "TMPDIR"

-- | Get a list of files in the specified directory, excluding "." and ".."
--
-- > ghci> listDirectory "/"
-- > ["home","sys","var","opt","lib64","sbin","usr","srv","dev","lost+found","bin","tmp","run","root","boot","proc","etc","lib"]
listDirectory
    :: RawFilePath -- ^ The path of directory to inspect
    -> IO [RawFilePath] -- ^ A list of files in the directory
listDirectory dirPath = filter f <$> getDirectoryFiles dirPath
  where
    f p = p /= "." && p /= ".."

-- | Get a list of files in the specified directory, including "." and ".."
--
-- > ghci> getDirectoryFiles "/"
-- > ["home","sys","var","opt","..","lib64","sbin","usr","srv","dev","lost+found","mnt","bin","tmp","run","root","boot",".","proc","etc","lib"]
getDirectoryFiles
    :: RawFilePath -- ^ The path of directory to inspect
    -> IO [RawFilePath] -- ^ A list of files in the directory
getDirectoryFiles dirPath = bracket open close repeatRead
  where
    open = U.openDirStream dirPath
    close = U.closeDirStream
    repeatRead stream = do
        d <- U.readDirStream stream
        if B.length d == 0 then return [] else do
            rest <- repeatRead stream
            return $ d : rest

-- | Recursively get all files in all subdirectories of the specified
-- directory.
--
-- > *System.RawFilePath> getDirectoryFilesRecursive "src"
-- > ["src/System/RawFilePath.hs"]
getDirectoryFilesRecursive
    :: RawFilePath -- ^ The path of directory to inspect
    -> IO [RawFilePath] -- ^ A list of relative paths
getDirectoryFilesRecursive path = do
    names <- map (path +/+) . filter (\x -> x /= ".." && x /= ".") <$>
        getDirectoryFiles path
    inspectedNames <- mapM inspect names
    return $ concat inspectedNames
  where
    inspect :: RawFilePath -> IO [RawFilePath]
    inspect p = fmap U.isDirectory (U.getFileStatus p) >>= \i -> if i
        then getDirectoryFilesRecursive p else return [p]

-- | Create a new directory.
--
-- > ghci> createDirectory "/tmp/mydir"
-- > ghci> getDirectoryFiles "/tmp/mydir"
-- > [".",".."]
-- > ghci> createDirectory "/tmp/mydir/anotherdir"
-- > ghci> getDirectoryFiles "/tmp/mydir"
-- > [".","..","anotherdir"]
createDirectory :: RawFilePath -> IO ()
createDirectory dir = U.createDirectory dir 0o755

-- | Create a new directory if it does not already exist.  If the first
-- argument is 'True' the function will also create all parent directories
-- when they are missing.
createDirectoryIfMissing
    :: Bool -- ^ Create parent directories or not
    -> RawFilePath -- ^ The path of the directory to create
    -> IO ()
createDirectoryIfMissing willCreateParents path
    | willCreateParents = createDirs parents
    | otherwise = createDir path ioError
  where
    createDirs []         = return ()
    createDirs [dir]   = createDir dir ioError
    createDirs (dir : dirs) = createDir dir $ \ _ ->
        -- Create parent directories (recursively) only when they are missing
        createDirs dirs >> createDir dir ioError
    createDir dir notExistHandler = tryIOError (createDirectory dir) >>= \ case 
        Right ()                   -> return ()
        Left  e
          | isDoesNotExistError  e -> notExistHandler e
          -- createDirectory (and indeed POSIX mkdir) does not distinguish
          -- between a dir already existing and a file already existing. So we
          -- check for it here. Unfortunately there is a slight race condition
          -- here, but we think it is benign. It could report an exeption in
          -- the case that the dir did exist but another process deletes the
          -- directory and creates a file in its place before we can check
          -- that the directory did indeed exist.  We also follow this path
          -- when we get a permissions error, as trying to create "." when in
          -- the root directory on Windows fails with
          --     CreateDirectory ".": permission denied (Access is denied.)
          -- This caused GHCi to crash when loading a module in the root
          -- directory.
          | isAlreadyExistsError e
         || isPermissionError    e -> do
              canIgnore <- catchIOError (pathIsDirectory dir) $ \ _ ->
                return (isAlreadyExistsError e)
              unless canIgnore (ioError e)
          | otherwise              -> ioError e
    parents = reverse $ scanl1 (+/+) $ B.split (w8 '/') $ stripSlash path

-- | Remove a file. This function internally calls @unlink@. If the file does
-- not exist, an exception is thrown.
removeFile :: RawFilePath -> IO ()
removeFile = U.removeLink

-- | A function that "tries" to remove a file. If the file does not exist,
-- nothing happens.
tryRemoveFile :: RawFilePath -> IO ()
tryRemoveFile path = catchIOError (U.removeLink path) $
    \ e -> unless (isDoesNotExistError e) $ ioError e

-- | Remove a directory. The target directory needs to be empty; Otherwise an
-- exception will be thrown.
removeDirectory :: RawFilePath -> IO ()
removeDirectory = U.removeDirectory

-- | Remove an existing directory /dir/ together with its contents and
-- subdirectories. Within this directory, symbolic links are removed without
-- affecting their targets.
removeDirectoryRecursive :: RawFilePath -> IO ()
removeDirectoryRecursive path =
  (`ioeAddLocation` "removeDirectoryRecursive") `modifyIOError` do
    m <- U.getSymbolicLinkStatus path
    case fileTypeFromMetadata m of
      Directory ->
        removeContentsRecursive path
      DirectoryLink ->
        ioError (err `ioeSetErrorString` "is a directory symbolic link")
      _ ->
        ioError (err `ioeSetErrorString` "not a directory")
  where err = mkIOError InappropriateType "" Nothing (Just (B8.unpack path))

-- | Remove an existing file or directory at /path/ together with its contents
-- and subdirectories. Symbolic links are removed without affecting their the
-- targets.
removePathRecursive :: RawFilePath -> IO ()
removePathRecursive path =
  (`ioeAddLocation` "removePathRecursive") `modifyIOError` do
    m <- U.getSymbolicLinkStatus path
    case fileTypeFromMetadata m of
      Directory     -> removeContentsRecursive path
      DirectoryLink -> U.removeDirectory path
      _             -> U.removeLink path

-- | Remove the contents of the directory /dir/ recursively. Symbolic links
-- are removed without affecting their the targets.
removeContentsRecursive :: RawFilePath -> IO ()
removeContentsRecursive path =
  (`ioeAddLocation` "removeContentsRecursive") `modifyIOError` do
    cont <- listDirectory path
    mapM_ removePathRecursive [path +/+ x | x <- cont]
    U.removeDirectory path


w8 :: Char -> Word8
w8 = fromIntegral . ord

stripSlash :: ByteString -> ByteString
stripSlash p = if B.last p == w8 '/' then B.init p else p

pathIsDirectory :: RawFilePath -> IO Bool
pathIsDirectory path = U.isDirectory <$> U.getFileStatus path


-- An extremely simplistic approach for path concatenation.
infixr 5  +/+
(+/+) :: RawFilePath -> RawFilePath -> RawFilePath
a +/+ b = mconcat [a, "/", b]
