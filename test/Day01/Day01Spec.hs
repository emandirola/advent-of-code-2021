module Day01.Day01Spec (spec) where

import Day01.Day01
import Test.Hspec

spec :: Spec
spec = do
  describe "Day01" $ do
    it "empty list" $ do
      countAscendingNumbers [] `shouldBe` 0
    it "one number" $ do
      countAscendingNumbers [1] `shouldBe` 0
    it "ascending number" $ do
      countAscendingNumbers [1, 2, 3, 4] `shouldBe` 3
    it "descending number" $ do
      countAscendingNumbers [4, 3, 2, 1] `shouldBe` 0
    it "ascending and descending number" $ do
      countAscendingNumbers [1, 2, 3, 4, 3, 2, 1] `shouldBe` 3
    it "random number" $ do
      countAscendingNumbers [1, 3, 5, 2, 4, 6, 3, 6] `shouldBe` 5
    it "test input" $ do
      countAscendingNumbers [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] `shouldBe` 7
      
      

