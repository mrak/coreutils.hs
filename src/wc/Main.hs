{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Args as A
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import Data.Maybe (isJust, fromJust)
import Control.Applicative

data Result = Result
    (Maybe Int64)
    (Maybe Int)
    (Maybe Int64)
    (Maybe Int64)
    (Maybe Int64)

defaultResult :: Result
defaultResult = Result
    (Just 0)
    (Just 0)
    (Just 0)
    (Just 0)
    (Just 0)

instance Show Result where
    show (Result ls ws cs bs ll) = concat . map (pad 8 ' ' . show . fromJust) $ filter isJust [ls,(fromIntegral <$> ws),cs,bs,ll]

main :: IO ()
main = do
    args <- A.getArgs
    case A.files args of
         Nothing -> B.getContents >>= (print . wc args) >> putStrLn ""
         Just fs -> mapM_ func fs
            where func f = wcFile args f >>= putResult f
                  putResult f r = putStrLn $ concat [show r," ",f]

wcFile :: A.Args -> FilePath -> IO (Result)
wcFile a f = fmap (wc a) (B.readFile f)

labelResults :: [Result] -> [FilePath] -> [String]
labelResults rs fs = zipWith (\r f -> show r ++ " " ++ f) (rs ++ [tally rs]) (fs ++ ["total"])
    where tally = foldr (addResults) defaultResult
          addResults (Result ls1 ws1 cs1 bs1 ll1) (Result ls2 ws2 cs2 bs2 ll2) =
              Result
              ((+) <$> ls1 <*> ls2)
              ((+) <$> ws1 <*> ws2)
              ((+) <$> cs1 <*> cs2)
              ((+) <$> bs1 <*> bs2)
              ((+) <$> ll1 <*> ll2)

wc :: A.Args -> B.ByteString -> Result
wc a xs = let ls = if A.lines a then Just $ linecount xs
                                else Nothing
              ws = if A.words a then Just $ wordcount xs
                                else Nothing
              cs = if A.chars a then Just $ charcount xs
                                else Nothing
              bs = if A.bytes a then Just $ bytecount xs
                                else Nothing
              ll = if A.longest a then Just $ longest xs
                                  else Nothing
          in Result ls ws cs bs ll

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

pad :: Int -> Char -> String -> String
pad w c s = (replicate n c) ++ s
    where n = w - length s
