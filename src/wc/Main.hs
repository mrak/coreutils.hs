{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Args as A (Args(..), getArgs)
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
    args <- A.getArgs
    L.interact $ wc args

wc :: A.Args -> L.ByteString -> L.ByteString
wc a xs = let ls = if A.lines a then disp $ linecount xs
                                else ""
              ws = if A.words a then disp $ wordcount xs
                                else ""
              cs = if A.chars a then disp $ charcount xs
                                else ""
              bs = if A.bytes a then disp $ bytecount xs
                                else ""
              ll = if A.longest a then disp $ longest xs
                                  else ""
          in  L.concat [ls,ws,cs,bs,ll,"\n"]

linecount :: L.ByteString -> Int64
linecount = L.count '\n'

wordcount :: L.ByteString -> Int
wordcount = length . L.words

charcount :: L.ByteString -> Int64
charcount = T.length . E.decodeUtf8

bytecount :: L.ByteString -> Int64
bytecount = L.length

longest :: L.ByteString -> Int
longest = maximum . map length . lines . L.unpack

disp :: Show a => a -> L.ByteString
disp = pad 8 ' ' . L.pack . show

pad :: Int64 -> Char -> L.ByteString -> L.ByteString
pad w c s = L.append (L.replicate n c) s
    where n = w - L.length s
