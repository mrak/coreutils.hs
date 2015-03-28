module Args (Args(..), getArgs) where

import Prelude hiding (words,lines)
import Options.Applicative

data Args = Args { bytes :: Bool
                 , chars :: Bool
                 , lines :: Bool
                 , words :: Bool
                 , longest :: Bool
                 , files :: Maybe [String]
                 , files0from :: Maybe FilePath
                 } deriving Show

bytesFlag :: Parser Bool
bytesFlag = switch ( long "bytes"
                   <> short 'c'
                   <> help "print the byte counts"
                   )

charsFlag :: Parser Bool
charsFlag = switch ( long "chars"
                   <> short 'm'
                   <> help "print the character counts"
                   )

linesFlag :: Parser Bool
linesFlag = switch ( long "lines"
                   <> short 'l'
                   <> help "print the newline counts"
                   )

wordsFlag :: Parser Bool
wordsFlag = switch ( long "words"
                   <> short 'w'
                   <> help "print the word counts"
                   )

longestFlag :: Parser Bool
longestFlag = switch ( long "max-line-length"
                     <> short 'L'
                     <> help "print the length of the longest line"
                     )

fileArgs :: Parser (Maybe [String])
fileArgs = optional $ some $ argument str (metavar "FILES...")

files0fromArg :: Parser (Maybe FilePath)
files0fromArg = optional $ strOption ( long "files0-from"
                                     <> help "read input from the files specified by NUL-terminated names in file F; If F is - then read names from standard input"
                                     )

options :: Parser Args
options =
    Args
    <$> bytesFlag
    <*> charsFlag
    <*> linesFlag
    <*> wordsFlag
    <*> longestFlag
    <*> fileArgs
    <*> files0fromArg

getArgs :: IO Args
getArgs = do
    a <- execParser $ info (helper <*> options) ( fullDesc <> header "")
    if bytes a || words a || lines a || chars a || longest a
       then return a
       else return a { bytes = True, words = True, lines = True }
