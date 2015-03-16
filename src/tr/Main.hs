module Main where

import Tr
import Args

main :: IO ()
main = getArgs >>= tr
