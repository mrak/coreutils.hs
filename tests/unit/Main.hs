import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Wc
import Data.List (intersperse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [wcTests]

wcTests = testGroup "wc" [splitTests]

splitTests = testGroup "split"
    [ testProperty "removes a" $
      \as -> length (as :: String) > 10 ==>
             (',' `elem` (concat . Wc.split ',' . intersperse ',' $ as)) /= True
    , testProperty "has +1 results from element count" $
      \as -> length (as :: String) > 10 ==>
             let commafied = intersperse ',' as
                 count = length . filter (== ',') $ commafied
                 len = length $ Wc.split ',' commafied
              in len == count + 1
    ]
