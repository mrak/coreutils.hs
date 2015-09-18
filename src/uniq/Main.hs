module Main where

import Args
import Uniq

main :: IO ()
main = getArgs >>= uniq
