module RawFilePath where

import System.IO
import qualified Data.ByteString as B

xprm :: IO ()
xprm = do
    (_, Just hout, _, ph) <- createProcessInternal ProcessConf
        { cmdargs = ["ls", "/"]
        , cwd = Nothing
        , env = Nothing
        , cfgStdin = NoStream
        , cfgStdout = CreatePipe
        , cfgStderr = NoStream
        , closeFds = False
        , createGroup = False
        , delegateCtlc = False
        , createNewConsole = False
        , newSession = False
        , childGroup = Nothing
        , childUser = Nothing
        }
    b <- B.hGetContents hout
    B.hPut stdout b

