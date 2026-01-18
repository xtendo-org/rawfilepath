{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B hiding (writeFile)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.RawFilePath as B
import Data.Semigroup
import RawFilePath hiding (ProcessConf)
import System.Exit
import System.IO
import System.Posix.Env.ByteString
import System.Posix.Temp.ByteString (mkdtemp)
import Test.Hspec

cWorkersDefault :: Int
cWorkersDefault = 8

cWorkersCI :: Int
cWorkersCI = 64

cIterationsDefault :: Int
cIterationsDefault = 8

cIterationsCI :: Int
cIterationsCI = 64

main :: IO ()
main = hspec $
  describe "RawFilePath.Process" $ do
    it "runs echo and reads stdout" $ do
      p <- startProcess $ proc "echo" ["hello"] `setStdout` CreatePipe
      result <- B.hGetContents (processStdout p)
      _ <- waitForProcess p
      result `shouldBe` "hello\n"

    it "handles concurrent process IO safely" $ do
      (cWorkers, cIterations) <-
        getEnv "GITHUB_ACTIONS" >>= \case
          Just v | not (B.null v) -> return (cWorkersCI, cIterationsCI)
          _ -> return (cWorkersDefault, cIterationsDefault)

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

    it "searches PATH from env when command has no slash" $
      withTempDir $ \tmpDir -> do
        let binDir = tmpDir <> "/bin"
            cmd = "rfp_execvpe_cmd"
            scriptPath = mconcat [binDir, "/", cmd]
        createDirectory binDir
        writeScript scriptPath "A"
        withEnvVars
          [ ("PATH", binDir)
          , ("RFP_TEST_VAR", "path")
          ]
          $ do
            (exitCode, stdoutB, stderrB) <-
              readProcessWithExitCode (proc cmd [])
            exitCode `shouldBe` ExitSuccess
            stderrB `shouldBe` ""
            stdoutB `shouldBe` "A:path"

    it "runs absolute path without consulting PATH" $
      withTempDir $ \tmpDir -> do
        let binA = tmpDir <> "/binA"
            binB = tmpDir <> "/binB"
            cmd = "rfp_execvpe_cmd"
            scriptA = binA <> "/" <> cmd
            scriptB = binB <> "/" <> cmd
        createDirectory binA
        createDirectory binB
        writeScript scriptA "A"
        writeScript scriptB "B"
        withEnvVars
          [ ("PATH", binB)
          , ("RFP_TEST_VAR", "abs")
          ]
          $ do
            (exitCode, stdoutB, stderrB) <-
              readProcessWithExitCode (proc scriptA [])
            exitCode `shouldBe` ExitSuccess
            stderrB `shouldBe` ""
            stdoutB `shouldBe` "A:abs"
 where
  trySome :: IO a -> IO (Either SomeException a)
  trySome = try

  withEnvVars :: [(ByteString, ByteString)] -> IO a -> IO a
  withEnvVars vars action = bracket setup restore (const action)
   where
    setup = do
      saved <- forM vars $ \(key, _) -> do
        value <- getEnv key
        return (key, value)
      forM_ vars $ \(key, value) -> setEnv key value True
      return saved
    restore saved =
      forM_ saved $ \(key, value) ->
        maybe (unsetEnv key) (\val -> setEnv key val True) value

  withTempDir :: (RawFilePath -> IO a) -> IO a
  withTempDir = bracket acquire removeDirectoryRecursive
   where
    acquire = do
      base <- getTemporaryDirectory
      let template = B.concat [base, "/rawfilepath-temp-XXXXXX"]
      mkdtemp template

  writeScript :: RawFilePath -> ByteString -> IO ()
  writeScript path label = do
    B.writeFile path (scriptBody label)
    exitCode <- callProcess (proc "/bin/chmod" ["+x", path])
    exitCode `shouldBe` ExitSuccess

  scriptBody :: ByteString -> ByteString
  scriptBody label =
    B.concat
      [ "#!/bin/sh\n"
      , "printf '%s' \""
      , label
      , ":$RFP_TEST_VAR\"\n"
      ]

  mkPayload workerId iter =
    build $
      mconcat
        [ B.intDec workerId
        , ":"
        , B.intDec iter
        , ":"
        , stimes (256 :: Int) "x"
        ]

build :: Builder -> ByteString
build = LB.toStrict . B.toLazyByteString
