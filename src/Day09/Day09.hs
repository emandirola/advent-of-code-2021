{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Day09.Day09 where

import Data.List (sortBy)
import System.TimeIt (timeItNamed)

input :: IO [[Int]]
input = map (map ((subtract (fromEnum '0')) . fromEnum)) . lines <$> readFile "input/Day09.txt"

lowerPoints :: [[Int]] -> [Int]
lowerPoints xs = go
  where
    sizex = length $ head xs
    sizey = length xs
    neighbors' = neighbors sizex sizey
    go = go' [0..sizex * sizey - 1] [] $ concat xs
    go' queue lower ys =
      if null queue then lower
      else let
        next = head queue
        next' = ys !! next
        isLower = all (> next') $ map (ys !!) $ neighbors' next
        lower' = if isLower then next:lower else lower
        queue' = tail queue
      in go' queue' lower' ys

neighbors :: Int -> Int -> Int -> [Int]
neighbors sizex sizey i = map (\(x', y') -> x' + sizex * y') $
  filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < sizex && y' < sizey)
  [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    where
      (x, y) = (i `mod` sizex, i `div` sizex)

basin :: [[Int]] -> Int -> [Int]
basin xs x = go [x] []
  where
    sizex = length $ head xs
    sizey = length xs
    xs' = concat xs
    go queue visited =
      if null queue then visited
      else let
        next = head queue
        q' = visited ++ queue 
        queue' = filter (\n -> xs' !! n /= 9 && n `notElem` q') $ neighbors sizex sizey next
      in go (queue' ++ tail queue) (next:visited)

part01 :: [[Int]] -> Int
part01 !xs = sum $ map (+1) $ map (ys!!) $ lowerPoints xs
  where
    ys = concat xs

part02 :: [[Int]] -> Int
part02 !xs = product $ take 3 $ sortBy (flip compare) $ map length $ map (basin xs) $ lowerPoints xs

day09 :: IO ()
day09 = do
  board <- input
  putStrLn "Day 09"
  board `seq` putStrLn "Board"
  putStrLn "Part 01"
  timeItNamed "part 01" $ putStrLn $ show $ part01 board
  putStrLn "Part 02"
  timeItNamed "part 02" $ putStrLn $ show $ part02 board
  return ()