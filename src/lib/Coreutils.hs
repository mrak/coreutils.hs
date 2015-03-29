module Coreutils where

split :: Eq a => a -> [a] -> [[a]]
split e s
  | null s    = []
  | s == [e]  = [[],[]]
  | otherwise = p : split e s' where
      (p,s') = case break (== e) s of
                    (x,[]) -> (x, [])
                    (x,xs) -> (x, tail xs)

escapeSequences :: [(String,String)]
escapeSequences = []
unescape :: String -> String
unescape = undefined
