{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import RawFilePath
import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "RawFilePath.Process" $
      it "runs echo and reads stdout" $ do
        p <- startProcess $ proc "echo" ["hello"] `setStdout` CreatePipe
        result <- B.hGetContents (processStdout p)
        _ <- waitForProcess p
        result `shouldBe` "hello\n"
