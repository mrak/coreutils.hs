{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString.Lazy.Char8 as L
import Args

main :: IO ()
main = do
    args <- getArgs
    L.interact $ wc args

wc :: Args -> L.ByteString -> L.ByteString
wc _ xs = let ls = linecount xs
              ws = wordcount xs
              cs = charcount xs
              bs = bytecount xs
              ll = longest xs
          in  L.concat [ls,ws,cs,bs,ll]

linecount :: L.ByteString -> L.ByteString
linecount = L.pack . show . L.count '\n'

wordcount :: L.ByteString -> L.ByteString
wordcount = L.pack . show . length . L.words

charcount :: L.ByteString -> L.ByteString
charcount = L.pack . show . length . L.unpack

bytecount :: L.ByteString -> L.ByteString
bytecount = L.pack . show . L.length

longest :: L.ByteString -> L.ByteString
longest = L.pack . show . maximum . map L.length . L.lines
