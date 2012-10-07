module Logger where

import Logger.Arguments
import System (getArgs)
import System.IO
import Control.Monad (forever)
import Network
import Data.Time.Clock
import Data.Time.Calendar


-- Process the arguments, and obtain a port and output file from them. If none
-- exist, default arguments are supplied (stdout for the file and 8808 for the
-- port).
defaultMain :: IO ()
defaultMain = withSocketsDo $ do 
    args   <- getArgs
    handle <- parseOr parseFile args (return stdout)
    port   <- parseOr parsePort args (return (PortNumber 8808))
    socket <- listenOn port
    forever $ logClientMessage socket handle
    sClose socket

currentTime :: IO (Int, Int, Int, Int, Int, Int)
currentTime = do 
    time <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay time
        (totalMin, sec)    = (`divMod` 60) . fromEnum . floor . toRational $ utctDayTime time
        (hour,  min)       = totalMin `divMod` 24
    return (fromInteger year :: Int, month, day, hour, min, sec)
                
-- Given a socket and a handle to write to, will receive the message from the
-- socket and write it to the handle.
logClientMessage :: Socket -> Handle -> IO ()
logClientMessage socket outH =  do
    (inH, host, _)   <- accept socket
    m                <- hGetContents inH
    time <- currentTime
    hPutStrLn outH (host ++ show time ++ ": " ++ m)
    hFlush outH
    hClose inH
