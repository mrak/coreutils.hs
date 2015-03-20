module Main where

import Wc
import Args

main :: IO ()
main = getArgs >>= wc
