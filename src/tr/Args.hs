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
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Char (ord, chr)
import Coreutils (unescape, unrange)

data CLIArgs = CLIArgs { complement :: Bool
                       , delete :: Bool
                       , squeeze :: Bool
                       , truncate :: Bool
                       , set1 :: String
                       , set2 :: Maybe String
                       } deriving Show

data Args = Args { squeezeSet :: SqueezeSet
                 , operation :: Operation
                 }

data Operation = Translate TranslateSet
               | Delete DeleteSet
               | Noop
               deriving (Eq)

type WordSet = Array Word8 Word8
type BoolSet = Array Word8 Bool
type TranslateSet = WordSet
type DeleteSet = BoolSet
type SqueezeSet = BoolSet

defaultDeleteSet :: BoolSet
defaultDeleteSet = array (0, 255) [(x, False) | x <- [0..255]]

defaultSqueezeSet :: BoolSet
defaultSqueezeSet = array (0, 255) [(x, False) | x <- [0..255]]

defaultTranslateSet :: WordSet
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
complementify c = if complement c then complementify' c else c where
    complementify' c' = c' {set1 = [chr x | x <- [0..255],  chr x `notElem` s]}
    s = set1 c

truncatify :: CLIArgs -> CLIArgs
truncatify c = case set2 c of
                    Nothing -> c
                    Just s2 -> truncatify' where
                        truncatify' = if truncate c then c {set1 = take (length s2) (set1 c)}
                                                    else c

expandify :: CLIArgs -> CLIArgs
expandify c = case set2 c of
                   Nothing -> c
                   Just s2 -> c {set2 = Just $ take (length s1) (s2 ++ repeat (last s2))} where
                       s1 = set1 c

createTranslationSet :: String -> String -> TranslateSet
createTranslationSet s1 s2 = defaultTranslateSet // zip (w8ify s1) (w8ify s2)

createBoolSet :: BoolSet -> String -> BoolSet
createBoolSet a s = a // zip (w8ify s) (repeat True)

createSqueezeSet :: Bool -> CLIArgs -> SqueezeSet
createSqueezeSet b a = createBoolSet defaultSqueezeSet s
  where s = if b then set1 a
                 else fromMaybe (error "Two strings must be given when both deleting and squeezing repeats.") (set2 a)

createDeleteSet :: String -> DeleteSet
createDeleteSet = createBoolSet defaultDeleteSet

unrangeify :: CLIArgs -> CLIArgs
unrangeify c = c { set1 = unrange (set1 c), set2 = unrange <$> set2 c }

unescapeify :: CLIArgs -> CLIArgs
unescapeify c = c { set1 = unescape (set1 c), set2 = unescape <$> set2 c }

processArgs :: CLIArgs -> IO Args
processArgs c' = do
    let c = expandify . truncatify . complementify . unrangeify . unescapeify $ c'
    let op = if delete c
        then Delete . createDeleteSet $ set1 c
        else case set2 c of
                  Nothing -> Noop
                  Just s  -> Translate . createTranslationSet (set1 c) $ s
    let sqzset = if squeeze c
        then createSqueezeSet (op == Noop) c
        else defaultSqueezeSet

    if op == Noop && not (squeeze c)
       then error "Two strings must be given while translating."
       else return Args {operation = op, squeezeSet = sqzset}

getArgs :: IO Args
getArgs = execParser parser >>= processArgs where
    parser = info (helper <*> options) (fullDesc <> header "")
