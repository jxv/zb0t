module Main (main) where

import System.Environment (getProgName,getArgs)
import Zb0t.Parse.Config (makeConfig)
import Zb0t.Core (run)


main :: IO ()
main = do
     args <- getArgs
     either printUsageErr run (makeConfig args)


printUsageErr :: String -> IO ()
printUsageErr err = do
     putStrLn $ "Error: " ++ err
     prog <- getProgName
     putStrLn $ unwords ("Usage:" : prog : options)


options :: [String]
options = 
    [ "-s <server>"
    , "[-p <port>]"
    , "-n <nick>"
    , "[-w <password>]"
    , "[-c <chan0> [<chan1> [...]]]"
    ]
