module Logger.Outputter where

import System.IO
import Network
import Data.Time.Clock
import Data.Time.Calendar

class Outputter a where 

    -- Given a socket and a handle to write to, will receive the message from the
    -- socket and write it to the handle.
    logClientMessage :: a -> Socket -> Handle -> IO ()
    logClientMessage _ socket outH = do
        (inH, host, _)   <- accept socket
        m                <- hGetContents inH
        time <- currentTime
        hPutStrLn outH (host ++ show time ++ ": " ++ m)
        hFlush outH
        hClose inH

currentTime :: IO (Int, Int, Int, Int, Int, Int)
currentTime = do 
    time <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay time
        (totalMin, sec)    = (`divMod` 60) . fromEnum . floor . toRational $ utctDayTime time
        (hour,  min)       = totalMin `divMod` 24
    return (fromInteger year :: Int, month, day, hour, min, sec)

data DefaultLogger = DefaultLogger

instance Outputter DefaultLogger
