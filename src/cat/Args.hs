module Args (
    Args(..)
    , getArgs
    ) where

import Options.Applicative

data CLIArgs = CLIArgs { _showAll :: Bool
                       , _numberNonblank :: Bool
                       , _showEndsAndNonprinting :: Bool
                       , _showEnds :: Bool
                       , _number :: Bool
                       , _squeezeBlank :: Bool
                       , _showTabsAndNonprinting :: Bool
                       , _showTabs :: Bool
                       , _showNonprinting :: Bool
                       , _files :: Maybe [FilePath]
                       }

data Args = Args { showEnds :: Bool
                 , numberLines :: NumberLines
                 , showTabs :: Bool
                 , showNonprinting :: Bool
                 , squeezeBlank :: Bool
                 , files :: [FilePath]
                 }

defaultArgs :: Args
defaultArgs = Args { showEnds = False
                   , numberLines = NoLines
                   , showTabs = False
                   , showNonprinting = False
                   , squeezeBlank = False
                   , files = ["-"]
                   }

data NumberLines = NoLines
                 | AllLines
                 | NonemptyLines

showAllFlag :: Parser Bool
showAllFlag = switch ( long "show-all"
                     <> short 'A'
                     <> help "equivalent to -vET"
                     )

numberNonblankFlag :: Parser Bool
numberNonblankFlag = switch ( long "number-nonblank"
                            <> short 'b'
                            <> help "number nonempty output lines, overrides -n"
                            )

showEndsAndNonprintingFlag :: Parser Bool
showEndsAndNonprintingFlag = switch ( short 'e'
                                    <> help "equivalent to -vE"
                                    )

showEndsFlag :: Parser Bool
showEndsFlag = switch ( long "show-ends"
                      <> short 'E'
                      <> help "display $ at end of each line"
                      )

numberFlag :: Parser Bool
numberFlag = switch ( long "number"
                    <> short 'n'
                    <> help "number all output lines"
                    )

squeezeBlankFlag :: Parser Bool
squeezeBlankFlag = switch ( long "squeeze-blank"
                          <> short 's'
                          <> help "supress repeated empty output lines"
                          )

showTabsAndNonprintingFlag :: Parser Bool
showTabsAndNonprintingFlag = switch ( short 't'
                                    <> help "equivalent to -vT"
                                    )

showTabsFlag :: Parser Bool
showTabsFlag = switch ( long "show-tabs"
                      <> short 'T'
                      <> help "display TAB characters at ^I"
                      )

showNonprintingFlag :: Parser Bool
showNonprintingFlag = switch ( long "show-nonprinting"
                             <> short 'v'
                             <> help "use ^ and M- notation, except for LFD and TAB"
                             )

fileArgs :: Parser (Maybe [FilePath])
fileArgs = optional $ some $ argument str (metavar "FILES...")

processShown :: CLIArgs -> Args -> Args
processShown c = showA . showe . showE . showt . showT . showv where
    showA a = if _showAll c
                 then a { showNonprinting = True, showEnds = True, showTabs = True }
                 else a
    showe a = if _showEndsAndNonprinting c
                 then a { showNonprinting = True, showEnds = True }
                 else a
    showE a = if _showEnds c
                 then a { showEnds = True }
                 else a
    showt a = if _showTabsAndNonprinting c
                 then a { showNonprinting = True, showTabs = True }
                 else a
    showT a = if _showTabs c
                 then a { showTabs = True }
                 else a
    showv a = if _showNonprinting c
                 then a { showNonprinting = True }
                 else a

processLines :: CLIArgs -> Args -> Args
processLines c = nonblankLines . allLines where
    allLines a = if _number c
                    then a { numberLines = AllLines }
                    else a
    nonblankLines a = if _numberNonblank c
                            then a { numberLines = NonemptyLines }
                            else a

processArgs :: CLIArgs -> IO Args
processArgs c = return . processLines c . processShown c $ defaultArgs

options :: Parser CLIArgs
options =
    CLIArgs
    <$> showAllFlag
    <*> numberNonblankFlag
    <*> showEndsAndNonprintingFlag
    <*> showEndsFlag
    <*> numberFlag
    <*> squeezeBlankFlag
    <*> showTabsAndNonprintingFlag
    <*> showTabsFlag
    <*> showNonprintingFlag
    <*> fileArgs

getArgs :: IO Args
getArgs = execParser parser >>= processArgs where
    parser = info (helper <*> options) (fullDesc <> header "")
