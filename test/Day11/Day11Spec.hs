module Day11.Day11Spec where

import Test.Hspec
import Day11.Day11 (step, part02, part01)
import Data.Foldable (foldl')

input :: [Int]
input = concat [
    [1,1,1,1,1],
    [1,9,9,9,1],
    [1,9,9,9,1],
    [1,9,9,9,1],
    [1,1,1,1,1]
  ]

step1 :: [Int]
step1 = concat [
    [3,4,5,4,3],
    [4,0,0,0,4],
    [5,0,0,0,5],
    [4,0,0,0,4],
    [3,4,5,4,3]
    ]

input2 :: [Int]
input2 = concat [
  [5,4,8,3,1,4,3,2,2,3],
  [2,7,4,5,8,5,4,7,1,1],
  [5,2,6,4,5,5,6,1,7,3],
  [6,1,4,1,3,3,6,1,4,6],
  [6,3,5,7,3,8,5,4,7,8],
  [4,1,6,7,5,2,4,6,4,5],
  [2,1,7,6,8,4,1,7,2,1],
  [6,8,8,2,8,8,1,1,3,4],
  [4,8,4,6,8,4,8,5,5,4],
  [5,2,8,3,7,5,1,5,2,6]]

exampleResult :: [Int]
exampleResult = concat [
  [0,3,9,7,6,6,6,8,6,6],
  [0,7,4,9,7,6,6,9,1,8],
  [0,0,5,3,9,7,6,9,3,3],
  [0,0,0,4,2,9,7,8,2,2],
  [0,0,0,4,2,2,9,8,9,2],
  [0,0,5,3,2,2,2,8,7,7],
  [0,5,3,2,2,2,2,9,6,6],
  [9,3,2,2,2,2,8,9,6,6],
  [7,9,2,2,2,8,6,8,6,6],
  [6,7,8,9,9,9,8,7,6,6]]

spec :: Spec
spec = do
    describe "Day 11" $ do
        it "should have correct step1" $ do
            step (0, input) `shouldBe` (9, step1)
        it "should have correct example" $ do
            part01 input2 `shouldBe` 1656
        it "should have correct part02" $ do
            part02 input2 `shouldBe` 195