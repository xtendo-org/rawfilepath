{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import RawFilePath
import System.IO
import Test.Hspec

cWorkers :: Int
cWorkers = 64

cIterations :: Int
cIterations = 10

main :: IO ()
main = hspec $
  describe "RawFilePath.Process" $ do
    it "runs echo and reads stdout" $ do
      p <- startProcess $ proc "echo" ["hello"] `setStdout` CreatePipe
      result <- B.hGetContents (processStdout p)
      _ <- waitForProcess p
      result `shouldBe` "hello\n"
    it "handles concurrent process IO safely" $ do
      let mkPayload workerId iter =
            B.pack $
              map (fromIntegral . fromEnum) $
                show workerId ++ ":" ++ show iter ++ ":" ++ replicate 256 'x'
      doneVars <-
        forM [1 .. cWorkers] $ \workerId -> do
          done <- newEmptyMVar
          _ <- forkIO $ do
            result <- trySome $ forM_ [1 .. cIterations] $ \iter -> do
              let payload = mkPayload workerId iter
              p <-
                startProcess $
                  proc "cat" []
                    `setStdin` CreatePipe
                    `setStdout` CreatePipe
              B.hPut (processStdin p) payload
              hClose (processStdin p)
              output <- B.hGetContents (processStdout p)
              _ <- waitForProcess p
              when (output /= payload) $
                throwIO (userError "output mismatch")
            putMVar done result
          return done
      results <- mapM takeMVar doneVars
      forM_ results $ \case
        Left err -> expectationFailure (displayException err)
        Right () -> return ()
 where
  trySome :: IO a -> IO (Either SomeException a)
  trySome = try
