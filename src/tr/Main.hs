module Main where
import Args

main :: IO ()
main = getArgs >>= validateArgs >>= tr

validateArgs :: Args -> IO Args
validateArgs args | otherwise = return args

tr :: Args -> IO ()
tr args = do
    let (t,ting) = case set2 args of
                   Just (s2) -> if   delete args
                                then (id, False)
                                else (translator (set1 args) (s2), False)
                   Nothing   -> (id, False) -- error here
    let (d,ding) = if   delete args
                   then (deleter, True)
                   else (id, False)
    let s = if   not $ squeeze args
            then id
            else squeezer (set1 args)
    print args

squeezer :: String -> String -> String
squeezer set []       = []
squeezer set (x:[])   = x:[]
squeezer set (x:y:ss) = if   x == y && x `elem` set
                        then y:(squeezer set ss)
                        else x:y:(squeezer set ss)

translator :: String -> String -> String -> String
translator s1 s2 _ = ""

deleter :: String -> String
deleter _ = ""
