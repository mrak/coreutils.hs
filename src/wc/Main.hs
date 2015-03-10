{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Int
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Args as A
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import Data.Maybe (isJust, fromJust)
import Control.Applicative
import Data.Monoid
import Data.List (intersperse)

main :: IO ()
main = do
    args <- A.getArgs
    case A.files args of
         Nothing -> print . wc args =<< B.getContents
         Just fs -> putStr . unlines . labelResults . appendTotal =<< wcFiles args fs

data Result =
    Result
    (Maybe Int64)
    (Maybe Int64)
    (Maybe Int64)
    (Maybe Int64)
    (Maybe Int64)

type FileResult = (FilePath,Result)

instance Monoid Result where
    mempty = Result (Just 0) (Just 0) (Just 0) (Just 0) (Just 0)
    mappend (Result ls1 ws1 cs1 bs1 ll1) (Result ls2 ws2 cs2 bs2 ll2) =
        Result
        ((+) <$> ls1 <*> ls2)
        ((+) <$> ws1 <*> ws2)
        ((+) <$> cs1 <*> cs2)
        ((+) <$> bs1 <*> bs2)
        ((+) <$> ll1 <*> ll2)

instance Show Result where
    show r = concat . maybePad $ filterShow r
      where maybePad (s:[]) = [s]
            maybePad ss = intersperse " " $ map (pad 7 ' ') ss

filterShow :: Result -> [String]
filterShow (Result ls ws cs bs ll) = map (show . fromJust) (filter isJust [ls,ws,cs,bs,ll])

wcFiles :: A.Args -> [FilePath] -> IO ([FileResult])
wcFiles args fs = mapM (wcFile args) fs
  where wcFile a f = fmap ((,) f) $ (fmap (wc a) (B.readFile f))

appendTotal :: [FileResult] -> [FileResult]
appendTotal (r:[]) = [r]
appendTotal rs = rs ++ [("total",total)]
    where total = foldr (<>) mempty (map (snd) rs)

labelResults :: [FileResult] -> [String]
labelResults frs = map (showWithFile padding) frs
  where padding = calcPadding frs

showWithFile :: Int -> FileResult -> String
showWithFile p (f,r) = (concat . maybePad $ filterShow r) ++ " " ++ f
  where maybePad (s:[]) = [s]
        maybePad ss = intersperse " " $ map (pad p ' ') ss

calcPadding :: [FileResult] -> Int
calcPadding rs = foldr (foldfn) 0 rs
    where foldfn fr a = maximum $ a:(map (length) (filterShow $ snd fr))

wc :: A.Args -> B.ByteString -> Result
wc a xs = let ls = if A.lines a then Just (linecount xs) else Nothing
              ws = if A.words a then Just (wordcount xs) else Nothing
              cs = if A.chars a then Just (charcount xs) else Nothing
              bs = if A.bytes a then Just (bytecount xs) else Nothing
              ll = if A.longest a then Just (longest xs) else Nothing
           in Result ls ws cs bs ll

linecount :: B.ByteString -> Int64
linecount = B.count '\n'

wordcount :: B.ByteString -> Int64
wordcount = fromIntegral . length . B.words

charcount :: B.ByteString -> Int64
charcount = T.length . E.decodeUtf8

bytecount :: B.ByteString -> Int64
bytecount = B.length

longest :: B.ByteString -> Int64
longest = maximum . map T.length . T.lines . E.decodeUtf8

pad :: Int -> Char -> String -> String
pad w c s = (replicate n c) ++ s
  where n = w - length s
