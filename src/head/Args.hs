module Args (Args(..), getArgs, Operation(..)) where

import Options.Applicative
import Data.Maybe (fromJust, fromMaybe)

data CLIArgs = CLIArgs { bytes :: Maybe Int
                       , lines :: Maybe Int
                       , quiet :: Bool
                       , verbose :: Bool
                       , filez :: [FilePath]
                       } deriving Show

data Args = Args { operation :: Operation
                 , showHeaders :: Bool
                 , files :: [FilePath]
                 } deriving Show

data Operation = Lines Int | Bytes Int | LastLines Int | LastBytes Int deriving Show

linesArg :: Parser (Maybe Int)
linesArg = optional $ option auto ( long "lines"
                                  <> short 'n'
                                  <> metavar "[-]K"
                                  <> help "print the first K lines instead of the first 10; with the leading '-', print all but the last K lines of each file")

bytesArg :: Parser (Maybe Int)
bytesArg = optional $ option auto ( long "bytes"
                                  <> short 'c'
                                  <> metavar "[-]K"
                                  <> help "print the first K bytes of each file; with the leading '-', print all but the last K bytes of each file")

quietFlag :: Parser Bool
quietFlag = switch ( long "quiet"
                   <> long "silent"
                   <> short 'q'
                   <> help "never print headers giving file names")

verboseFlag :: Parser Bool
verboseFlag = switch ( long "verbose"
                     <> short 'v'
                     <> help "always print headers giving file names")

fileArgs :: Parser [FilePath]
fileArgs = fromMaybe ["-"] <$> optional (some $ argument str (metavar "FILE..."))

options :: Parser CLIArgs
options =
    CLIArgs
    <$> bytesArg
    <*> linesArg
    <*> quietFlag
    <*> verboseFlag
    <*> fileArgs


processOperation :: CLIArgs -> Operation
processOperation c = fromJust $ fromLines <$> Args.lines c <|> fromBytes <$> bytes c <|> Just (Lines 10)

fromLines :: Int -> Operation
fromLines i | i < 0 = LastLines (abs i)
            | otherwise = Lines i

fromBytes :: Int -> Operation
fromBytes b | b < 0 = LastBytes (abs b)
            | otherwise = Bytes b


processArgs :: CLIArgs -> IO Args
processArgs c = pure Args { operation = processOperation c
                          , showHeaders = verbose c || (not . quiet) c && (length . filez) c > 1
                          , files = filez c
                          }

getArgs :: IO Args
getArgs = execParser parser >>= processArgs where
    parser = info (helper <*> options) (fullDesc <> header "")
