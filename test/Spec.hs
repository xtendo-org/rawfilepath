{-# language OverloadedStrings #-}

import RawFilePath
import qualified Data.ByteString as B

main :: IO ()
main = do
    p <- startProcess $ proc "echo" ["hello"] `setStdout` CreatePipe
    result <- B.hGetContents (processStdout p)
    _ <- waitForProcess p

    print (result == "hello\n")
