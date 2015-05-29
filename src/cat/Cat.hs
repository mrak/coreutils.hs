{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Cat where

import Prelude as P hiding (putStrLn, getContents, readFile)
import Args
import Data.Int
import Data.ByteString.Lazy.Char8 as B
import Data.Monoid
import Control.Monad

cat :: Args -> IO ()
cat a = B.putStr . displayify . numberify =<< appendFiles (files a) where
    displayify = if showEnds a
                    then displayEnds
                    else id
    numberify = case numberLines a of
                     NoLines -> id
                     AllLines -> numberAll
                     NonemptyLines -> numberNonblank

appendFiles :: [FilePath] -> IO B.ByteString
appendFiles = fmap (P.foldr1 (<>)) . mapM getFileContents

getFileContents :: FilePath -> IO B.ByteString
getFileContents "-" = B.getContents
getFileContents f = B.readFile f

displayEnds :: B.ByteString -> B.ByteString
displayEnds = B.unlines . dollarify . B.lines where
    dollarify = fmap (`B.append` "$")

numberNonblank :: B.ByteString -> B.ByteString
numberNonblank = B.unlines . number 1 . B.lines where
    number :: Int -> [B.ByteString] -> [B.ByteString]
    number _ [] = []
    number n (b:bs) | B.length b == 0 = b : number n bs
                    | otherwise = (pad 6 ' ' n) <> "  " <> b : number (n + 1) bs

numberAll :: B.ByteString -> B.ByteString
numberAll = B.unlines . number 1 . B.lines where
    number :: Int -> [B.ByteString] -> [B.ByteString]
    number _ [] = []
    number n (b:bs) = (pad 6 ' ' n) <> "  " <> b : number (n + 1) bs

pad :: Show a => Int64 -> Char -> a -> B.ByteString
pad w c a = B.replicate n c <> s where
    s = pack $ show a
    n = w - B.length s
