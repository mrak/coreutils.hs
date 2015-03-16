module Main where

import Wc
import qualified Args as A

main :: IO ()
main = do
    args <- A.getArgs
    case A.files0from args of
         Just f   -> files0from f >>= doFiles args
         Nothing  -> case A.files args of
                          Nothing -> doStdin args
                          Just fs -> doFiles args fs
