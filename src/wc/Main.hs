{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Args as A (Args(..), getArgs)
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import Control.Applicative

main :: IO ()
main = do
    args <- A.getArgs
    case A.files args of
         Nothing -> B.interact (wc args) >> B.putStrLn ""
         Just fs -> mapM_ func fs
            where func f = wc args <$> B.readFile f >>= putResult f
                  putResult f b = B.putStrLn $ B.concat [b, " ", B.pack f]

wc :: A.Args -> B.ByteString -> B.ByteString
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
          in  trim . B.drop 1 . B.concat $ [ls,ws,cs,bs,ll]
          where trim bs = if   (length . B.words) bs == 1
                          then B.dropWhile isSpace bs
                          else bs
                isSpace = (== ' ')

linecount :: B.ByteString -> Int64
linecount = B.count '\n'

wordcount :: B.ByteString -> Int
wordcount = length . B.words

charcount :: B.ByteString -> Int64
charcount = T.length . E.decodeUtf8

bytecount :: B.ByteString -> Int64
bytecount = B.length

longest :: B.ByteString -> Int64
longest = maximum . map T.length . T.lines . E.decodeUtf8

disp :: Show a => a -> B.ByteString
disp = pad 8 ' ' . B.pack . show

pad :: Int64 -> Char -> B.ByteString -> B.ByteString
pad w c s = B.append (B.replicate n c) s
    where n = w - B.length s
