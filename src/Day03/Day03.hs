module Day03.Day03 where

import Data.List (sort, transpose, group, maximumBy, foldl', sortBy, minimumBy)
import Data.Function (on, fix)
import Control.Arrow ((&&&))
import Debug.Trace (trace)
import Data.Ord (comparing)

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

-- part 2

type RepFinder = Int -> [String] -> Char

mostRepeatedInColumn :: RepFinder
mostRepeatedInColumn n = head . maximumBy (comparing length) . group . sort . (!!n) . transpose

leastRepeatedInColumn :: RepFinder
leastRepeatedInColumn n = head . minimumBy (comparing length) . group . sort . (!!n) . transpose

finderRec :: RepFinder -> Int -> [String] -> [String]
finderRec rf n xs'
  | length xs' == 1 = xs'
  | otherwise = finderRec rf (n+1) $ filterp2 n xs' $ rf n xs'
  
oxigenGeneratorRating :: [String] -> Int
oxigenGeneratorRating = binToDec . map (=='1') . head . finderRec mostRepeatedInColumn 0

co2ScrubberRating :: [String] -> Int
co2ScrubberRating = binToDec . map (=='1') . head . finderRec leastRepeatedInColumn 0

filterp2 :: Int -> [String] -> Char -> [String]
filterp2 n xs c = filter (\xs' -> xs' !! n == c) xs

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
  print "Part 2"
  print (oxigenGeneratorRating input, co2ScrubberRating input)
  print $ oxigenGeneratorRating input * co2ScrubberRating input