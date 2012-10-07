module Logger where

import Logger.Arguments
import Logger.Outputter
import System.Environment (getArgs)
import System.IO
import Control.Monad (forever)
import Network

-- Process the arguments, and obtain a port and output file from them. If none
-- exist, default arguments are supplied (stdout for the file and 8808 for the
-- port).
defaultMain :: IO ()
defaultMain = withSocketsDo $ do 
    args   <- getArgs
    handle <- parseOr parseFile args (return stdout)
    port   <- parseOr parsePort args (return (PortNumber 8808))
    socket <- listenOn port
    forever $ logClientMessage DefaultLogger socket handle
    sClose socket

