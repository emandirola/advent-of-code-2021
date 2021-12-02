module Day02.Day02Spec (spec) where

import Day02.Day02
import Test.Hspec

spec :: Spec
spec = do
  describe "Day02" $ do
    it "part 1" $ do
      move1 ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"] `shouldBe` (15, 10)
    it "part 2" $ do
      let testCase = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
      print $ map parse testCase
      print $ scanl1 (\x y -> (fst x + fst y, snd x + (snd y * fst x))) $ map parse testCase
      move2 ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"] `shouldBe` (15, 60)
