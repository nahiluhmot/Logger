-- This file contains the server side for the logging service.
import Control.Monad (forever)
import Network
import System (getArgs)
import System.IO
import System.Directory (doesFileExist)
import Text.ParserCombinators.Parsec

-- Process the arguments, and obtain a port and output file from them. If none
-- exist, default arguments are supplied (stdout for the file and 8808 for the
-- port).
main :: IO ()
main = withSocketsDo $ do 
    args   <- getArgs
    handle <- parseOr parseFile args (return stdout)
    port   <- parseOr parsePort args (return (PortNumber 8808))
    socket <- listenOn port
    forever $ logClientMessage socket handle
    sClose socket

-- Attempt to parse the xs one by one, and on failure, return def. This function
-- is very useful for processing command line arguments.
parseOr :: Parser a -> [String] -> a -> a
parseOr _ []     def = def
parseOr p (x:xs) def = 
    case parse p "" x of
        Left  _ -> parseOr p xs def
        Right y -> y

-- Will match anything that looks like "-p{Port}" or "--port={Port}". The
-- port must be an integer. If the string is parseable, it will return a new 
-- IO PortID.
parsePort :: Parser (IO PortID)
parsePort = do
    string "-p" <|> string "--port="
    p <- many1 digit
    return . return . PortNumber $ fromIntegral (read p :: Int)
               
-- Will match anything that looks like "-f{File}" or "--filename={File}". If 
-- the file passed in doesn't exist, it will create a new one.
parseFile :: Parser (IO Handle)
parseFile = do 
    string "-f" <|> string "--filename="
    fileName <- many1 anyChar
    return $ do
        exists <- doesFileExist fileName
        openFile fileName $ if exists then AppendMode else WriteMode
                
-- Given a socket and a handle to write to, will receive the message from the
-- socket and write it to the handle.
logClientMessage :: Socket -> Handle -> IO ()
logClientMessage socket outH =  do
    (inH, _, _)      <- accept socket
    m                <- hGetContents inH
    hPutStrLn outH m
    hFlush outH
    hClose inH
