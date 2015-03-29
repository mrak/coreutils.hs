module Args (
    Args(..)
    , TranslateSet
    , SqueezeSet
    , DeleteSet
    , getArgs
    , Operation(..)
    ) where

import Prelude hiding (truncate)
import Options.Applicative
import Data.Array
import Data.Word
import Data.Char (ord, chr)

data CLIArgs = CLIArgs { complement :: Bool
                       , delete :: Bool
                       , squeeze :: Bool
                       , truncate :: Bool
                       , set1 :: String
                       , set2 :: Maybe String
                       } deriving Show

data Args = Args { squeezeSet :: Array Word8 Bool
                 , operation :: Operation
                 }

data Operation = Translate TranslateSet
               | Delete DeleteSet
               | Noop
               deriving (Eq)

type TranslateSet = Array Word8 Word8
type BoolSet = Array Word8 Bool
type DeleteSet = BoolSet
type SqueezeSet = BoolSet

defaultDeleteSet :: Array Word8 Bool
defaultDeleteSet = array (0, 255) [(x, False) | x <- [0..255]]

defaultSqueezeSet :: Array Word8 Bool
defaultSqueezeSet = array (0, 255) [(x, False) | x <- [0..255]]

defaultTranslateSet :: Array Word8 Word8
defaultTranslateSet = array (0, 255) [(x, x) | x <- [0..255]]

complementFlag :: Parser Bool
complementFlag = switch ( long "complement"
                        <> short 'C'
                        <> short 'c'
                        <> help "use the complement of SET1"
                        )

deleteFlag :: Parser Bool
deleteFlag = switch ( long "delete"
                    <> short 'd'
                    <> help "delete characters in SET1, do not translate"
                                                           )

squeezeFlag :: Parser Bool
squeezeFlag = switch ( long "squeeze-repeats"
                     <> short 's'
                     <> help "replace each input sequence of a repeated character that is listed in SET1 with a single occurrence of that character"
                     )

truncateFlag :: Parser Bool
truncateFlag = switch ( long "truncate-set1"
                      <> short 't'
                      <> help "first truncate SET1 to length of SET2"
                      )

set1Arg :: Parser String
set1Arg = argument str (metavar "SET1")

set2Arg :: Parser (Maybe String)
set2Arg = optional $ argument str (metavar "SET2")

options :: Parser CLIArgs
options =
    CLIArgs
    <$> complementFlag
    <*> deleteFlag
    <*> squeezeFlag
    <*> truncateFlag
    <*> set1Arg
    <*> set2Arg

w8ify :: String -> [Word8]
w8ify = map c2w8

c2w8 :: Char -> Word8
c2w8 = fromIntegral . ord

complementify :: CLIArgs -> CLIArgs
complementify c = c {set1 = [(chr x) | x <- [0..255], not (elem (chr x) s)]} where
    s = set1 c

truncatify :: CLIArgs -> CLIArgs
truncatify c = case set2 c of
                    Nothing -> c
                    Just s2 -> c {set1 = take (length s2) (set1 c)}

createTranslationSet :: String -> String -> TranslateSet
createTranslationSet s1 s2 = defaultTranslateSet // zip (w8ify s1) (w8ify s2)

createBoolSet :: BoolSet -> String -> BoolSet
createBoolSet a s = a // zip (w8ify s) (repeat True)

createSqueezeSet :: Bool -> CLIArgs -> SqueezeSet
createSqueezeSet b a = createBoolSet defaultSqueezeSet s
  where s = if b then set1 a
                 else case set2 a of
                           Nothing -> error "Two strings must be given when both deleting and squeezing repeats."
                           Just s2 -> s2

createDeleteSet :: String -> DeleteSet
createDeleteSet = createBoolSet defaultDeleteSet

processArgs :: CLIArgs -> IO Args
processArgs c' = do
    let c = truncatify . complementify $ c'
    let op = if delete c
        then Delete . createDeleteSet $ set1 c
        else case set2 c of
                  Nothing -> Noop
                  Just s  -> Translate . createTranslationSet (set1 c) $ s
    let sqzset = if squeeze c
        then createSqueezeSet (op == Noop) c
        else defaultSqueezeSet

    if op == Noop && squeeze c == False
       then error "Two strings must be given while translating."
       else return $ Args {operation = op, squeezeSet = sqzset}

getArgs :: IO Args
getArgs = execParser parser >>= processArgs where
    parser = info (helper <*> options) (fullDesc <> header "")
