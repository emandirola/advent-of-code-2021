module Day01.Day01 where

-- https://adventofcode.com/2021/day/1

{-
countAscendingNumbers :: [Int] -> Int
countAscendingNumbers [] = 0
countAscendingNumbers [_] = 0
countAscendingNumbers (x1:xs@(x2:_))
  | x2 > x1 = 1 + countAscendingNumbers xs
  | otherwise = countAscendingNumbers xs
  -}

countAscendingNumbers :: [Int] -> Int
countAscendingNumbers = length . filter id . (zipWith (<) <*> tail)

countAscendingWindow :: Int -> [Int] -> Int
countAscendingWindow window =
  countAscendingNumbers . foldr1 (zipWith (+)) . take window . iterate (drop 1)

day01 :: IO ()
day01 = do
  numbers <- map read . lines <$> readFile "input/day01.txt"
  print $ countAscendingNumbers numbers
  print $ countAscendingWindow 3 numbers
