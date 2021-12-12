module Day06.Day06 where

import Data.List (foldl')


part01 :: [Int] -> [Int]
part01 fish = foldl' go fish [1..80]
  where
    go :: [Int] -> Int -> [Int]
    go fs _ = concat $ map go' fs
      where
        go' :: Int -> [Int]
        go' f = if f > 0 then [f - 1] else [6, 8]

day06 :: IO ()
day06 = do
  fish <- read <$> (\s -> "[" ++ s ++ "]") <$> readFile "input/day06.txt"
  putStrLn "Day 06"
  putStrLn "Part 1"
  putStrLn $ show $ length $ part01 fish
