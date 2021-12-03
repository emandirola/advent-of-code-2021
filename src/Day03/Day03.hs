module Day03.Day03 where

import Data.List (sort, transpose, group, maximumBy)
import Data.Function (on)

merge :: (Int, Int) -> Char -> (Int, Int)
merge (a, b) '0' = (a + 1, b)
merge (a, b) '1' = (a, b + 1)
merge ab _ = ab

gammaToBinary :: [String] -> [Bool]
gammaToBinary = map (uncurry (<=) . foldl merge (0, 0)) . transpose

gammaToBinary' :: [String] -> [Bool]
gammaToBinary' = map (head . maximumBy (compare `on` length) . group . sort . map (== '1')) . transpose

epsilonToBinary :: [Bool] -> [Bool]
epsilonToBinary = map not

binToDec :: [Bool] -> Int
binToDec = foldl (\x y -> fromEnum y + 2 * x) 0

day03 :: IO ()
day03 = do
  print "Day 03"
  input <- lines <$> readFile "input/day03.txt"
  print "Part 1"
  let gamma = gammaToBinary' input
  let gammaDec = binToDec gamma
  let epsilonDec = binToDec $ epsilonToBinary gamma
  print (gammaDec, epsilonDec)
  print $ gammaDec * epsilonDec
