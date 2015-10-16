module Main where

import Head as H
import Args

main :: IO ()
main = getArgs >>= H.head
