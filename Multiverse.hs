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

parseOr :: Parser a -> [String] -> a -> a
parseOr _ []     def = def
parseOr p (x:xs) def = case parse p "" x of
                               Left _  -> parseOr p xs def
                               Right y -> y

parsePort :: Parser (IO PortID)
parsePort = do
    string "-p" <|> string "--portname="
    p <- many1 digit
    return . return . PortNumber $ fromIntegral (read p :: Int)
               
parseFile :: Parser (IO Handle)
parseFile = do 
    string "-f" <|> string "--filename="
    f <- many1 anyChar
    return (openFileForLogging f)

openFileForLogging :: FilePath -> IO Handle
openFileForLogging fileName = do
    exists <- doesFileExist fileName
    openFile fileName (if exists then AppendMode else WriteMode)

--logClientMessage :: PortID -> Handle -> IO ()
logClientMessage socket outH =  do
    (inH, _, _)      <- accept socket
    m                <- hGetContents inH
    hPutStrLn outH m
    hFlush outH
    hClose inH
