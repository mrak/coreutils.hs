{-# LANGUAGE OverloadedStrings #-}
module Base64 where

import Pipes
import qualified Pipes.Prelude as P
import Data.ByteString.Base64.Lazy
import Data.ByteString.Lazy.Char8 as B
import qualified Args as A
import Data.Int (Int64)
import Control.Monad (unless)

base64 :: A.Args -> IO ()
base64 a = runEffect $ source a >-> operation a >-> wrap a >-> sink

source :: A.Args -> Producer B.ByteString IO ()
source a = let f = A.file a
            in if f == "-"
                  then lift B.getContents >>= yield
                  else (lift . B.readFile) f >>= yield

sink :: Consumer B.ByteString IO ()
sink = P.mapM_ B.putStrLn

operation :: A.Args -> Pipe B.ByteString B.ByteString IO ()
operation a = await >>=
    yield . case A.mode a of
                 A.Encode -> encode
                 A.Decode -> strategy . B.filter (/= '\n') where
                     strategy = if A.ignoreGarbage a then decodeLenient else either error id . decode

wrap :: A.Args -> Pipe B.ByteString B.ByteString IO ()
wrap a = case A.mode a of
              A.Decode -> P.map id
              _ -> case A.wrap a of
                        A.NoWrap -> P.map id
                        A.WrapAt c -> await >>= wrapAt (fromIntegral c)

wrapAt :: Int64 -> B.ByteString -> Pipe B.ByteString B.ByteString IO ()
wrapAt c bs = unless (B.null bs) $ do
    yield $ B.take c bs
    wrapAt c $ B.drop c bs
