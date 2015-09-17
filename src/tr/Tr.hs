module Tr where

import Args
import Data.Array
import Data.Char (ord, chr)
import Data.Word
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as B

tr :: Args -> IO ()
tr args = runEffect $ getChars >-> operationFn >-> squeeze set >-> putChars where
    set = squeezeSet args
    operationFn = case operation args of
                       Translate s -> translate s
                       Delete s -> delete s
                       Noop -> P.map id

getChars :: Producer Char IO ()
getChars = lift B.getContents >>= getChars' where
    getChars' c = case B.uncons c of
                       Nothing -> return ()
                       Just(w,bs) -> yield w >> getChars' bs

putChars :: Consumer Char IO ()
putChars = P.mapM_ putChar

squeeze :: Monad m => SqueezeSet -> Pipe Char Char m r
squeeze ss = do
    a <- await
    yield a
    b <- await
    squeeze' a b where
    squeeze' l x | x /= l = yield x >> await >>= squeeze' x
                 | otherwise = if ss ! toIx x
                                  then await >>= squeeze' x
                                  else yield x >> await >>= squeeze' x

delete :: DeleteSet -> Pipe Char Char IO ()
delete s = P.filter (\w -> not (s ! toIx w))

translate :: TranslateSet -> Pipe Char Char IO ()
translate s = P.map (\w -> chr . fromIntegral $ s ! toIx w)

toIx :: Char -> Word8
toIx = fromIntegral . ord
