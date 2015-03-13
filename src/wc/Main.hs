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
import Data.Char (chr)
import System.IO (stdin,hGetContents)

main :: IO ()
main = do
    args <- A.getArgs
    case A.files0from args of
         Just "-" -> hGetContents stdin >>= doFiles args . split nul
         Nothing  -> case A.files args of
                          Nothing -> doStdin args
                          Just fs -> doFiles args fs
         Just f   -> doFiles args =<< readFilenames f

doStdin :: A.Args -> IO ()
doStdin args = print . wc args =<< B.getContents
doFiles :: A.Args -> [FilePath] -> IO ()
doFiles args fs = putStr . unlines . label fs . total . map (wc args) =<< readFiles fs

readFilenames :: FilePath -> IO [FilePath]
readFilenames f = fmap (split nul) (readFile f)

data Result =
    Result
    (Maybe Int64)
    (Maybe Int64)
    (Maybe Int64)
    (Maybe Int64)
    (Maybe Int64)

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
      where maybePad [s] = [s]
            maybePad ss = intersperse " " $ map (pad 7 ' ') ss

filterShow :: Result -> [String]
filterShow (Result ls ws cs bs ll) = map (show . fromJust) (filter isJust [ls,ws,cs,bs,ll])

readFiles :: [FilePath] -> IO [B.ByteString]
readFiles = mapM B.readFile

total :: [Result] -> [Result]
total [r] = [r]
total rs = rs ++ [foldr (<>) mempty rs]

label :: [FilePath] -> [Result] -> [String]
label fs rs = map (showFileResult padding) frPairs
  where padding = widest rs
        frPairs = zip (fs ++ ["total"]) rs

showFileResult :: Int -> (FilePath,Result) -> String
showFileResult p (f,r) = (concat . maybePad $ filterShow r) ++ " " ++ f
  where maybePad [s] = [s]
        maybePad ss = intersperse " " $ map (pad p ' ') ss

widest :: [Result] -> Int
widest = foldr foldfn 0
  where foldfn r a = maximum $ a : map length (filterShow r)

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
pad w c s = replicate n c ++ s
  where n = w - length s

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
                 "" -> []
                 s' -> w : split c s''
                   where (w, s'') = break (== c) s'

nul :: Char
nul = chr 0
