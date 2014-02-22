module Main (main) where

---

import Zb0t.Imports
import Zb0t.Config (makeConfig)
import Zb0t.Core (run)

----

main :: IO ()
main =
  do args <- getArgs
     let econfig = makeConfig args
     either usageErr run econfig

usageErr :: String -> IO ()
usageErr err =
  do putStrLn ("Error: " ++ err ++ "\n")
     prog <- getProgName
     putStrLn ("Usage:\t" ++ prog ++ " -s <server> [-p <port>] -n <nick> [-w <password>] [-c <chan0> [<chan1> [...]]]\n")


