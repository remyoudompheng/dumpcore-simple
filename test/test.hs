module Main where

import Control.Lens
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit
import Functions

main :: IO ()
main = do
    defaultMain tests

caseLens :: Assertion
caseLens = do
    let l = [T (show x) x | x <- [0..30]]
    sum (toListOf (traverse.world) $ incWorld l) @?= 496

tests :: TestTree
tests =
    testGroup ""
    [ testCase "hello lenses!" caseLens
    ]


