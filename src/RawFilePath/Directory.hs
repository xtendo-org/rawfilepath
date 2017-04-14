module RawFilePath.Directory
    ( listDirectory
    , getDirectoryFiles
    , createDirectory
    , createDirectoryIfMissing
    ) where

import RawFilePath.Import

-- extra modules

import qualified Data.ByteString as B
import qualified System.Posix.ByteString as U -- U for Unix

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
createDirectoryIfMissing :: Bool -> RawFilePath -> IO ()
createDirectoryIfMissing
    willCreateParents -- ^ Create parent directories or not
    path -- ^ The path of the directory to create
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
