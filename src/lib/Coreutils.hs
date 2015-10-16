{-# LANGUAGE OverloadedStrings #-}
module Coreutils ( unescape
                 , unrange
                 , split
                 , bsContents
                 , bsContents'
                 , bsUnlines
                 , bsUnlines'
                 , versionOption
                 ) where

import Data.Char (chr, isOctDigit, ord)
import Options.Applicative
import Numeric (readOct)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as B'
import Data.Version (showVersion, Version)

bsContents :: FilePath -> IO B.ByteString
bsContents "-" = B.getContents
bsContents f   = B.readFile f

bsContents' :: FilePath -> IO B'.ByteString
bsContents' "-" = B'.getContents
bsContents' f   = B'.readFile f

bsUnlines :: [B.ByteString] -> B.ByteString
bsUnlines = B.intercalate "\n"

bsUnlines' :: [B'.ByteString] -> B'.ByteString
bsUnlines' = B'.intercalate "\n"

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

versionOption :: String -> Version -> Parser (a -> a)
versionOption n v =
    infoOption (n ++ " (Mrak coreutils) " ++ showVersion v)
    ( long "version"
    <> help "output version information and exit"
    )

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
