import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Wc
import Data.List (intersperse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
    [ testProperty "split removes a" $
      \as -> length (as :: String) > 10 ==>
             (',' `elem` (concat . Wc.split ',' . intersperse ',' $ as)) /= True
    ]
