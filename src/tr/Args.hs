module Args (Args(..), getArgs) where
import Options.Applicative

data CLIArgs = CLIArgs
    { complement :: Bool
    , delete :: Bool
    , squeeze :: Bool
    , truncate :: Bool
    , set1 :: String
    , set2 :: Maybe String
    } deriving Show

data Args = Args
    { mode :: Mode
    , squeeze_set :: Maybe String
    }

data Mode = Translate | Delete | None

complementFlag :: Parser Bool
complementFlag = switch
    ( long "complement"
   <> short 'C'
   <> short 'c'
   <> help "use the complement of SET1"
    )

deleteFlag :: Parser Bool
deleteFlag = switch
    ( long "delete"
   <> short 'd'
   <> help "delete characters in SET1, do not translate"
    )

squeezeFlag :: Parser Bool
squeezeFlag = switch
    ( long "squeeze-repeats"
   <> short 's'
   <> help "replace each input sequence of a repeated character that is listed in SET1 with a single occurrence of that character"
    )

truncateFlag :: Parser Bool
truncateFlag = switch
    ( long "truncate-set1"
   <> short 't'
   <> help "first truncate SET1 to length of SET2"
    )

set1Arg :: Parser String
set1Arg = argument str (metavar "SET1")

set2Arg :: Parser (Maybe String)
set2Arg = optional $ argument str (metavar "SET2")

options :: Parser CLIArgs
options = CLIArgs
    <$> complementFlag
    <*> deleteFlag
    <*> squeezeFlag
    <*> truncateFlag
    <*> set1Arg
    <*> set2Arg

validateArgs :: CLIArgs -> IO Args
validateArgs cas = return $ Args {mode=Translate, squeeze_set=Nothing}

getArgs :: IO Args
getArgs = execParser parser >>= validateArgs
    where parser = info (helper <*> options) (fullDesc <> header "")
