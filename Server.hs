import System.IO
import Network

main :: IO ()
main = withSocketsDo $ do
    socket <- listenOn (PortNumber 8808)
    (handle, hostname, portnumber) <- accept socket
    message <- hGetContents handle
    hClose handle
    putStrLn message
    sClose socket

