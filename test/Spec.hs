{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import RawFilePath

main :: IO ()
main = do
  p <- startProcess $ proc "echo" ["hello"] `setStdout` CreatePipe
  result <- B.hGetContents (processStdout p)
  _ <- waitForProcess p

  print (result == "hello\n")
