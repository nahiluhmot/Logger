import System
import System.IO
import Network
import Network.Socket hiding (accept)
import Control.Monad

main :: IO ()
main = do
    fileName <- fmap head getArgs
    file <- openFile fileName AppendMode
    putStrLn "listening on port 8808, press Ctrl+C to stop"
    forever $ logClientMessage (PortNumber 8808) file

logClientMessage ::  PortID -> Handle -> IO ()
logClientMessage port outH = withSocketsDo $ do
    socket           <- listenOn port
    (inH, _, _)      <- accept socket
    m                <- hGetContents inH
    hPutStrLn outH m
    hFlush outH
    hClose inH
    sClose socket


