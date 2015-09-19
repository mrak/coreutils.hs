module Yes where

import qualified Args as A
import Control.Monad (forever)

yes :: A.Args -> IO ()
yes a = forever . putStrLn $ case A.strings a of
                                  Nothing -> "y"
                                  Just ss -> unwords ss
