module Args (Args(..), getArgs) where
import Options.Applicative

data Args = Args
    { bytes :: Bool
    , chars :: Bool
    , lines :: Bool
    , words :: Bool
    , longest :: Bool
    } deriving Show

bytesFlag :: Parser Bool
bytesFlag = switch
    ( long "bytes"
   <> short 'c'
   <> help "print the byte counts"
    )

charsFlag :: Parser Bool
charsFlag = switch
    ( long "chars"
   <> short 'm'
   <> help "print the character counts"
    )

linesFlag :: Parser Bool
linesFlag = switch
    ( long "lines"
   <> short 'l'
   <> help "print the newline counts"
    )

wordsFlag :: Parser Bool
wordsFlag = switch
    ( long "words"
   <> short 'w'
   <> help "print the word counts"
    )

longestFlag :: Parser Bool
longestFlag = switch
    ( long "max-line-length"
   <> short 'L'
   <> help "print the length of the longest line"
    )

options :: Parser Args
options = Args
    <$> bytesFlag
    <*> charsFlag
    <*> linesFlag
    <*> wordsFlag
    <*> longestFlag

getArgs :: IO Args
getArgs = execParser $ info (helper <*> options)
    ( fullDesc
   <> header ""
    )
