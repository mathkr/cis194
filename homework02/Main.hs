module Main where

import Log
import LogAnalysis

main :: IO ()
main = do
    logs <- testParse parse 10000 "error.log"
    let sorted = inOrder $ build logs
    mapM_ (putStrLn . show) sorted
