module Args where

import Options.Applicative

data Args = Args { strings :: Maybe [String] }

stringArgs :: Parser (Maybe [String])
stringArgs = optional $ some $ argument str (metavar "STRINGS...")

options :: Parser Args
options = Args <$> stringArgs

getArgs :: IO Args
getArgs = execParser $ info (helper <*> options) (fullDesc <> header "")
