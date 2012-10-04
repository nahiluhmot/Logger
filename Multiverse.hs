{-# LANGUAGE FlexibleInstances #-}

import Default
import Data.List
import Data.Word
import Data.Maybe
import System
import System.IO
import System.Directory
import Network
import Network.Socket hiding (accept)
import Control.Monad

-- Process the arguments, and obtain a port and output file from them. If none
-- exist, default arguments are supplied (stdout for the file and 8808 for the
-- port).
main :: IO ()
main = do args   <- getArgs
          port   <- mapRun getPort args
          handle <- mapRun getFile args
          forever $ logClientMessage port handle

instance Default (IO Handle) where
    defaultValue = return stdout
instance Default (IO PortID) where
    defaultValue = return (PortNumber 8808)

getPort :: String -> Maybe (IO PortID)
getPort (    '-':'p':                port) = Just . return . PortNumber $ fromIntegral (read port :: Int)
getPort ('-':'-':'p':'o':'r':'t':'=':port) = Just . return . PortNumber $ fromIntegral (read port :: Int)
getPort _                                  = Nothing

getFile :: String -> Maybe (IO Handle)
getFile (    '-':'f':                                fileName) = Just $ openFileForLogging fileName
getFile ('-':'-':'f':'i':'l':'e':'n':'a':'m':'e':'=':fileName) = Just $ openFileForLogging fileName
getFile _                                                      = Nothing

openFileForLogging :: FilePath -> IO Handle
openFileForLogging fileName = do
    exists <- doesFileExist fileName
    openFile fileName (if exists then AppendMode else WriteMode)

logClientMessage :: PortID -> Handle -> IO ()
logClientMessage port outH = withSocketsDo $ do
    socket           <- listenOn port
    (inH, _, _)      <- accept socket
    m                <- hGetContents inH
    hPutStrLn outH m
    hFlush outH
    hClose inH
    sClose socket
