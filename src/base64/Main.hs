module Main where

import Args
import Base64

main :: IO ()
main = getArgs >>= base64
