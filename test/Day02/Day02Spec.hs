module Day02.Day02Spec (spec) where
  
-- https://adventofcode.com/2021/day/2

import Day02.Day02
import Test.Hspec


testCase :: [String]
testCase = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

spec :: Spec
spec = do
  describe "Day02" $ do
    it "part 1" $ do
      part1 testCase `shouldBe` (15, 10)
    it "part 2" $ do
      part2 testCase `shouldBe` (15, 60)
