module Main where

import Cat
import Args

main :: IO ()
main = getArgs >>= cat
