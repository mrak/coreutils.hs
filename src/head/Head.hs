{-# LANGUAGE OverloadedStrings #-}
module Head where

import qualified Args as A
import qualified Data.ByteString.Lazy.Char8 as B
import Coreutils (bsContents)

head :: A.Args -> IO ()
head a = mapM op (A.files a) >>= B.putStr . B.intercalate "\n" where
    op f = header f <$> case A.operation a of
                             A.Lines n -> firstLines f n
                             A.Bytes b -> firstBytes f b
                             A.LastLines n -> lastLines f n
                             A.LastBytes b -> lastBytes f b
    header f = B.append $ if A.showHeaders a
                             then "==> " `B.append` B.pack f `B.append` " <==\n"
                             else ""

firstLines :: FilePath -> Int -> IO B.ByteString
firstLines f i = bsContents f >>= pure . B.unlines . take i . B.lines where

firstBytes :: FilePath -> Int -> IO B.ByteString
firstBytes f i = bsContents f >>= pure . B.take (fromIntegral i) where

lastLines :: FilePath -> Int -> IO B.ByteString
lastLines f i = bsContents f >>= pure . B.unlines . reverse . drop i . reverse . B.lines where

lastBytes :: FilePath -> Int -> IO B.ByteString
lastBytes f i = bsContents f >>= pure . B.reverse . B.drop (fromIntegral i) . B.reverse where
