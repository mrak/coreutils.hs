{-# LANGUAGE ViewPatterns #-}
module Coreutils ( unescape
                 , unescapeBS
                 , unrange
                 , split
                 ) where

import Data.Char (chr, isOctDigit, ord)
import Numeric (readOct)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B

split :: Eq a => a -> [a] -> [[a]]
split e s
    | null s    = []
    | s == [e]  = [[],[]]
    | otherwise = p : split e s' where
        (p,s') = case break (== e) s of
                      (x,[]) -> (x, [])
                      (x,xs) -> (x, tail xs)

escapeSequences :: M.Map String Char
escapeSequences = M.fromList [("\\\\", '\\'),("\\a", chr 7),("\\b", chr 8),("\\f", chr 12),("\\n", chr 10),("\\r", chr 13),("\\t", chr 9),("\\v", chr 11)]

unrange :: String -> String
unrange [] = []
unrange s@[_] = s
unrange s@[_,_] = s
unrange (x:'\\':'-':cs) = x : '-' : unrange cs
unrange (x:'-':y:cs)
    | ord x >= ord y = x : '-' : y : unrange cs
    | otherwise = [chr c | c <- [(ord x)..(ord y)]] ++ unrange cs

unrange (h:t) = h : unrange t

unescape :: String -> String
unescape [] = []
unescape s@[_] = s
unescape ('\\':'-':zs) = '\\' : '-' : unescape zs
unescape ('\\':y:zs)
    | isOctDigit y = let os = (take 2 . takeWhile isOctDigit) zs
                         zs' = drop (length os) zs
                      in octToChar (y : os) : unescape zs'
    | otherwise = case M.lookup ['\\',y] escapeSequences of
                       Nothing -> unescape (y : zs)
                       Just c -> c : unescape zs

unescape s = head s : unescape (tail s)

octToChar :: String -> Char
octToChar = chr . fst . head . readOct


unescapeBS :: B.ByteString -> B.ByteString
unescapeBS (B.uncons -> Nothing) = B.empty
unescapeBS (B.uncons -> Just (x, B.uncons -> Nothing)) = B.singleton x
unescapeBS (B.uncons -> Just ('\\', B.uncons -> Just (y, zs))) =
    case M.lookup ['\\',y] escapeSequences of
         Nothing -> '\\' `B.cons` unescapeBS (y `B.cons` zs)
         Just c -> c `B.cons` unescapeBS zs

unescapeBS bs = B.head bs `B.cons` unescapeBS (B.tail bs)
