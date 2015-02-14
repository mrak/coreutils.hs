module Args (Args(..), getArgs) where
import Options.Applicative

data Args = Args
    { complement :: Bool
    , delete :: Bool
    , squeeze :: Bool
    , truncate :: Bool
    , set1 :: String
    , set2 :: Maybe String
    } deriving Show


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

options :: Parser Args
options = Args
    <$> complementFlag
    <*> deleteFlag
    <*> squeezeFlag
    <*> truncateFlag
    <*> set1Arg
    <*> set2Arg

getArgs :: IO Args
getArgs = execParser $ info (helper <*> options)
    ( fullDesc
   <> header ""
    )
