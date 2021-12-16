{-# LANGUAGE ScopedTypeVariables #-}

module Day07.Day07 where

import Data.List (sort, sortBy, findIndex)
import Data.Maybe (fromJust)

spread :: [Int] -> [Int]
spread xs = sortBy dmean [(mean - minds)..(mean + minds)]
  where
    min' = minimum xs
    max' = maximum xs
    minds = min (max' - mean) (mean - min')
    dmean a b = compare (abs (a - mean)) (abs (b - mean))
    mean = sum xs `div` length xs

part01 :: [Int] -> Int
part01 xs = minimum [go s xs | s <- spread']
  where
    go s xs' = sum $ map (abs . subtract s) xs'
    spread' = spread xs

p :: Int -> Int
p = (map p' [1..] !!)
  where
    p' n = n * (n-1) `div` 2

part02 :: [Int] -> Int
part02 xs = go mean xs
  where
    mean = sum xs `div` length xs
    go s = sum . map (diff s)
    diff x y = p $ abs $ subtract x y

input :: IO [Int]
input = read <$> (\x -> "[" ++ x ++ "]") <$> readFile "input/day07.example.txt" :: IO [Int]

day07 :: IO ()
day07 = do
  input' <- input
  putStrLn "Day 07"
  putStrLn "Part 01"
  putStrLn $ show $ part01 $ sort input'
  putStrLn "Part 02"
  putStrLn $ show $ part02 $ sort input'
  return ()