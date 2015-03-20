{-# LANGUAGE ViewPatterns #-}
module Tr where

import Args
import Data.Array
import Data.Char (ord, chr)
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as B

tr :: Args -> IO ()
tr args = B.putStr . squeeze (squeezeSet args) . operationFn =<< B.getContents
    where operationFn = case operation args of
                             Translate s -> translate s
                             Delete s -> delete s
                             Noop -> id

squeeze :: SqueezeSet -> B.ByteString -> B.ByteString
squeeze _ (B.uncons -> Nothing) = B.empty
squeeze _ (B.uncons -> Just (x, B.uncons -> Nothing)) = B.singleton x
squeeze a (B.uncons -> Just (x, B.uncons -> Just (y, zs)))
    | x /= y = x `B.cons` squeeze a (y `B.cons` zs)
    | otherwise = if   a!(toIx x) == True
                  then squeeze a $ y `B.cons` zs
                  else x `B.cons` squeeze a (y`B.cons` zs)

delete :: DeleteSet -> B.ByteString -> B.ByteString
delete _ (B.uncons -> Nothing) = B.empty
delete s (B.uncons -> Just (w, ws)) = if   s!(toIx w) == True
                                      then delete s ws
                                      else w `B.cons` delete s ws

translate :: TranslateSet -> B.ByteString -> B.ByteString
translate _ (B.uncons -> Nothing) = B.empty
translate s (B.uncons -> Just (w, ws)) = w' `B.cons` translate s ws
    where w' = chr . fromIntegral $ s!(toIx w)

toIx :: Char -> Word8
toIx = fromIntegral . ord
