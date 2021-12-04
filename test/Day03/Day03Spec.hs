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
      print $ gammaToBinary testCase
      (binToDec . gammaToBinary) testCase `shouldBe` 22

    it "oxigen subtest" $ do
      mostRepeatedInColumn 0 testCase `shouldBe` '1'
      let step1 = [ "11110", "10110", "10111", "10101", "11100", "10000", "11001" ]
      filterp2 0 testCase '1' `shouldBe` step1
      mostRepeatedInColumn 1 step1 `shouldBe` '0'
      let step2 = ["10110", "10111", "10101", "10000"]
      filterp2 1 step1 '0' `shouldBe` step2
      mostRepeatedInColumn 2 step2 `shouldBe` '1'
      let step3 = ["10110", "10111", "10101"]
      filterp2 2 step2 '1' `shouldBe` step3
      mostRepeatedInColumn 3 step2 `shouldBe` '1'
      let step4 = ["10110", "10111"]
      filterp2 3 step3 '1' `shouldBe` step4

    it "oxigen" $ do
      oxigenGeneratorRating testCase `shouldBe` 23

    it "co2" $ do
      co2ScrubberRating testCase `shouldBe` 10
