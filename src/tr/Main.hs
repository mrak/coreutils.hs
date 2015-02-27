module Main where
import Args

main :: IO ()
main = getArgs >>= tr

tr :: Args -> IO ()
tr _ = return ()
