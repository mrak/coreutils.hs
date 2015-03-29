import Test.Tasty
import Test.Tasty.QuickCheck
import Coreutils
import Data.List (intersperse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [libTests]

libTests :: TestTree
libTests = testGroup "coreutils" [splitTests]

splitTests :: TestTree
splitTests = testGroup "split"
    [ testProperty "removes a" $
      \as -> length (as :: String) > 10 ==>
             notElem ',' . concat . split ',' . intersperse ',' $ as
    , testProperty "has +1 results from element count" $
      \as -> length (as :: String) > 10 ==>
             let commafied = intersperse ',' as
                 count = length . filter (== ',') $ commafied
                 len = length $ split ',' commafied
              in len == count + 1
    ]
