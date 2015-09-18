module Args where

import Options.Applicative
import Data.Maybe (fromMaybe)
import Coreutils
import Paths_coreutils (version)

data Args = Args { mode :: Mode
                 , ignoreGarbage :: Bool
                 , wrap :: Wrap
                 , file :: FilePath
                 } deriving Show

data Mode = Decode | Encode deriving Show
data Wrap = WrapAt Int | NoWrap deriving Show

modeFlag :: Parser Mode
modeFlag = flag Encode Decode ( long "decode"
                              <> short 'd'
                              <> help "decode data"
                              )

fileArg :: Parser (Maybe FilePath)
fileArg = optional $ argument str ( metavar "FILE" )

garbageFlag :: Parser Bool
garbageFlag = flag False True ( long "ignore-garbage"
                              <> short 'i'
                              <> help "when decoding, ignore non-alphabet characters"
                              )

wrapArg :: Parser (Maybe Int)
wrapArg = optional $ option auto ( long "wrap"
                                 <> short 'w'
                                 <> metavar "COLS"
                                 <> help "wrap encoded lines after COLS character (default 76).  Use 0 to disable line wrapping"
                                 )

processWrap :: Maybe Int -> Wrap
processWrap = maybe (WrapAt 76) (\i -> if i < 1 then NoWrap else WrapAt i)

options :: Parser Args
options =
    Args
    <$> modeFlag
    <*> garbageFlag
    <*> fmap processWrap wrapArg
    <*> fmap (fromMaybe "-") fileArg

getArgs :: IO Args
getArgs = execParser $ info (versionOption "base64" version <*> helper <*> options) (fullDesc <> header "")
