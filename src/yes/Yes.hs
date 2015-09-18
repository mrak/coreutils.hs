module Yes where

import qualified Args as A
import Control.Monad (forever)

yes :: A.Args -> IO ()
yes a = do
    let s = case A.strings a of
                  Nothing -> "yes"
                  Just ss -> unwords ss
    forever $ putStrLn s
