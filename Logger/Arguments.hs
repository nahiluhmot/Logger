module Logger.Arguments where

import System.IO
import System.Directory (doesFileExist)
import Text.ParserCombinators.Parsec
import Network

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
