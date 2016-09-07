{-# language LambdaCase #-}

-- |
-- Module      : System.RawFilePath
-- Copyright   : (c) Kinoru 2016
-- License     : BSD-style
--
-- Maintainer  : xkinoru@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Higher-level API for the 'RawFilePath'-variants of functions in the 'unix'
-- module.

module System.RawFilePath
    ( RawFilePath
    -- * Process
    , callProcess
    , callProcessSilent
    , readProcess
    , readProcessEither
    -- * Directory
    , listDirectory
    , getDirectoryFiles
    , getDirectoryFilesRecursive
    , copyFile
    , getHomeDirectory
    , doesFileExist
    , doesDirectoryExist
    , setCurrentDirectory
    , tryRemoveFile
    ) where

import Data.Monoid
import Control.Monad
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import System.IO
import System.IO.Error
import System.Exit (ExitCode(..))

import Foreign.Marshal.Alloc (allocaBytes)
import System.Posix.ByteString

processError :: RawFilePath -> IOError
processError cmd = mkIOError userErrorType
    ("calling process " <> show cmd) Nothing (Just $ show cmd)

-- | Creates a new process to run the specified command with the given
-- arguments, and waits for it to finish. Throws an exception if the process
-- returns a nonzero exit code.
--
-- > *System.RawFilePath> callProcess "ls" ["-a", "src"]
-- > .  ..  System
callProcess
    :: RawFilePath -- ^ Command to run
    -> [ByteString] -- ^ Command arguments
    -> IO ()
callProcess cmd args = do
    pid <- forkProcess $ executeFile cmd True args Nothing
    getProcessStatus True False pid >>= \case
        Just status -> case status of
            Exited exitCode -> case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> failure
            _ -> failure
        Nothing -> failure
  where
    failure = ioError (processError cmd)

-- | Same as 'callProcess' except the child process will share the parent\'s
-- stdout and stderr, meaning it won\'t print anything.
callProcessSilent
    :: RawFilePath -- ^ Command to run
    -> [ByteString] -- ^ Command arguments
    -> IO ExitCode
callProcessSilent cmd args = do
    pid <- forkProcess $ do
        closeFd stdOutput
        closeFd stdError
        executeFile cmd True args Nothing
    getProcessStatus True False pid >>= \case
        Just status -> case status of
            Exited exitCode -> return exitCode
            _ -> failure
        Nothing -> failure
  where
    failure = ioError (processError cmd)

getContentsAndClose :: Handle -> IO ByteString
getContentsAndClose h = B.hGetContents h <* hClose h

-- | Runs a command, reads its standard output strictly, blocking until the process terminates, and returns the output as 'ByteString'.
--
-- > *System.RawFilePath> readProcess "date" ["+%s"]
-- > "1469999314\n"
readProcess
    :: RawFilePath -- ^ Command to run
    -> [ByteString] -- ^ Command arguments
    -> IO ByteString -- ^ The output from the command
readProcess cmd args = do
    (fd0, fd1) <- createPipe
    pid <- forkProcess $ do
        closeFd fd0
        closeFd stdOutput
        closeFd stdError
        void $ dupTo fd1 stdOutput
        executeFile cmd True args Nothing
    closeFd fd1
    (fdToHandle fd0 >>= getContentsAndClose) <*
        getProcessStatus True False pid

-- | A \'safer\' approach to 'readProcess'. Depending on the exit status of
-- the process, this function will return output either from stderr or stdout.
--
-- > *System.RawFilePath> readProcessEither "date" ["%s"]
-- > Left "date: invalid date \226\128\152%s\226\128\153\n"
-- > *System.RawFilePath> readProcessEither "date" ["+%s"]
-- > Right "1469999817\n"
readProcessEither
    :: RawFilePath -- ^ Command to run
    -> [ByteString] -- ^ Command arguments
    -> IO (Either ByteString ByteString) -- ^ Either the stedrr output from
    -- the command if it finished with a nonzero exit code, or the stdout data
    -- if it finished normally.
readProcessEither cmd args = do
    (fd0, fd1) <- createPipe
    (efd0, efd1) <- createPipe
    pid <- forkProcess $ do
        closeFd fd0
        closeFd stdOutput
        void $ dupTo fd1 stdOutput
        closeFd efd0
        closeFd stdError
        void $ dupTo efd1 stdError
        executeFile cmd True args Nothing
    closeFd fd1
    closeFd efd1
    content <- fdToHandle fd0 >>= getContentsAndClose
    err <- fdToHandle efd0 >>= getContentsAndClose
    getProcessStatus True False pid >>= \case
        Just status -> case status of
            Exited exitCode -> case exitCode of
                ExitSuccess -> return $ Right content
                ExitFailure _ -> return $ Left err
            _ -> return $ Left err
        Nothing -> return $ Left err

-- | Get a list of files in the specified directory, excluding "." and ".."
--
-- > *System.RawFilePath> listDirectory "src"
-- > ["..","System","."]
listDirectory
    :: RawFilePath -- ^ The path of directory to inspect
    -> IO [RawFilePath] -- ^ A list of files in the directory
listDirectory dirPath = filter f <$> getDirectoryFiles dirPath
  where
    f p = p /= "." && p /= ".."

-- | Get a list of files in the specified directory, including "." and ".."
--
-- > *System.RawFilePath> getDirectoryFiles "src"
-- > ["..","System","."]
getDirectoryFiles
    :: RawFilePath -- ^ The path of directory to inspect
    -> IO [RawFilePath] -- ^ A list of files in the directory
getDirectoryFiles dirPath = bracket open close repeatRead
  where
    open = openDirStream dirPath
    close = closeDirStream
    repeatRead stream = do
        d <- readDirStream stream
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
    names <- map (path </>) . filter (\x -> x /= ".." && x /= ".") <$>
        getDirectoryFiles path
    inspectedNames <- mapM inspect names
    return $ concat inspectedNames
  where
    inspect :: RawFilePath -> IO [RawFilePath]
    inspect p = fmap isDirectory (getFileStatus p) >>= \i -> if i
        then getDirectoryFilesRecursive p else return [p]

defaultFlags :: OpenFileFlags
defaultFlags = OpenFileFlags
    { append = False
    , exclusive = False
    , noctty = True
    , nonBlock = False
    , trunc = False
    }

-- Buffer size for file copy
bufferSize :: Int
bufferSize = 4096

-- | Copy a file from the source path to the destination path.
copyFile
    :: RawFilePath -- ^ The source path
    -> RawFilePath -- ^ The destination path
    -> IO ()
copyFile srcPath tgtPath = do
    bracket ropen hClose $ \hi ->
        bracket topen hClose $ \ho ->
            allocaBytes bufferSize $ copyContents hi ho
    rename tmpPath tgtPath
  where
    ropen = openFd srcPath ReadOnly Nothing defaultFlags >>= fdToHandle
    topen = createFile tmpPath stdFileMode >>= fdToHandle
    tmpPath = tgtPath <> ".copyFile.tmp"
    copyContents hi ho buffer = do
        count <- hGetBuf hi buffer bufferSize
        when (count > 0) $ do
            hPutBuf ho buffer count
            copyContents hi ho buffer

-- | A function that "tries" to remove a file. If the file does not exist,
-- nothing happens.
tryRemoveFile :: RawFilePath -> IO ()
tryRemoveFile path = catchIOError (removeLink path) $
    \e -> unless (isDoesNotExistError e) $ ioError e

-- | Returns the current user\'s home directory.
getHomeDirectory :: IO RawFilePath
getHomeDirectory = getEnv "HOME" >>= maybe err return
  where
    err = ioError $ mkIOError doesNotExistErrorType errMsg Nothing Nothing
    errMsg = "Environment variable $HOME"

-- | Returns 'True' if the argument file exists and is not a directory.
doesFileExist :: RawFilePath -> IO Bool
doesFileExist path = fileExist path >>= \i -> if i
    then not . isDirectory <$> getFileStatus path
    else return False

-- | Returns 'True' if the argument file exists and is a directory.
doesDirectoryExist :: RawFilePath -> IO Bool
doesDirectoryExist path = fileExist path >>= \i -> if i
    then isDirectory <$> getFileStatus path
    else return False

-- | Change the working directory to the given path.
setCurrentDirectory :: RawFilePath -> IO ()
setCurrentDirectory = changeWorkingDirectory

-- An extremely simplistic approach for path concatenation.
infixr 5  </>
(</>) :: RawFilePath -> RawFilePath -> RawFilePath
a </> b = mconcat [a, "/", b]
