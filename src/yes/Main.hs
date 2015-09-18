module Main where

import Yes
import Args

main :: IO ()
main = getArgs >>= yes
