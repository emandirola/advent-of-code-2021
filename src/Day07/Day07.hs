{-# LANGUAGE ScopedTypeVariables #-}

module Day07.Day07 where

import Data.List (sort, sortBy)

linearRegression :: [Double] -> [Double] -> Double
linearRegression xs ys = mean ys + variance xs ys / variance xs xs * (0 - mean xs)
  where
    n = fromIntegral $ length xs
    variance :: [Double] -> [Double] -> Double
    variance xs' ys' = ((mean xs'2) * (mean ys'2)) - ((mean xs ** 2) * (mean ys ** 2))
      where
        xs'2 = map (**2) xs'
        ys'2 = map (**2) ys'
    mean :: [Double] -> Double
    mean xs' = sum xs' / n

spread :: [Int] -> [Int]
spread xs = [x | x <- sortBy dmean [(mean - minds)..(mean + minds)]]
  where
    min' = minimum xs
    max' = maximum xs
    minds = min (max' - mean) (mean - min')
    dmean a b = compare (abs (a - mean)) (abs (b - mean))
    mean = sum xs `div` length xs

part01 :: [Int] -> Int
part01 xs = minimum [go s xs | s <- spread']
  where
    go s xs' = sum $ map (abs . (s-)) xs'
    spread' = spread xs

day07 :: IO ()
day07 = do
  input <- read <$> (\x -> "[" ++ x ++ "]") <$> readFile "input/day07.txt" :: IO [Int]
  putStrLn "Day 07"
  putStrLn "Part 01"
  putStrLn $ show $ part01 $ sort input
  return ()