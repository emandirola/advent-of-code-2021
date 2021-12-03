module Day03.Day03Spec where

import Day03.Day03
import Test.Hspec

testCase :: [String]
testCase = ["00100",
           "11110",
           "10110",
           "10111",
           "10101",
           "01111",
           "00111",
           "11100",
           "10000",
           "11001",
           "00010",
           "01010"]

spec :: Spec
spec = do
  describe "Day 03" $ do
    it "calculate gamma" $ do
      print $ gammaBin testCase
      (binToDec . gammaBin) testCase `shouldBe` 22
