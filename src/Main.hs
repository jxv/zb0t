module Main (main) where

import Network (withSocketsDo)
import System.Environment (getProgName,getArgs)
import Zb0t.Parse.Config (makeConfig)
import Zb0t.Core (run)
import qualified Zb0t.Monad as Zb0t


main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  either printUsageErr run (makeConfig args)
  Zb0t.runIO (return ())

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
