module Main where

import Wc
import qualified Args as A
import System.IO (stdin,hGetContents)

main :: IO ()
main = do
    args <- A.getArgs
    case A.files0from args of
         Just "-" -> hGetContents stdin >>= doFiles args . split nul
         Just f   -> doFiles args =<< readFilenames f
         Nothing  -> case A.files args of
                          Nothing -> doStdin args
                          Just fs -> doFiles args fs
