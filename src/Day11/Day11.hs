{-# LANGUAGE TupleSections #-}

module Day11.Day11 where

import Data.List (sort, group, foldl')
import Data.Maybe (fromMaybe)
import Data.Either (fromLeft)
import Data.Foldable (foldlM)

input :: IO [Int]
input = concatMap (map $ (subtract $ fromEnum '0') . fromEnum) . lines <$> readFile "input/day11.txt"

neighbors :: Int -> Int -> [Int]
neighbors size i = map (\(x', y') -> x' + size * y') $
  filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < size && y' < size)
  [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)]
    where
      (x, y) = (i `mod` size, i `div` size)

step :: (Int, [Int]) -> (Int, [Int])
step (flashes, m) =
  let
    size :: Int
    size = floor $ sqrt $ fromIntegral $ length m
    neighbors' = neighbors size
    go = go' (zip [0..length m] [1::Int,1..]) [] $ m
    go' queue flashed xs =
      let
        step1 = map (\(i, x) -> x + fromMaybe 0 (i `lookup` queue)) $ zip [0::Int ..] xs
        flashed' = map fst $ filter (\(i, x) -> i `notElem` flashed && x > 9) $ zip [0::Int ..] step1
        flashed'' = flashed ++ flashed'
        queue' = map (\ns -> (head ns, length ns)) $ group $ sort $ filter (`notElem` flashed'') $ concatMap neighbors' flashed'
        zeroed = map (\x -> if x > 9 then 0 else x) step1
        go'' = go' queue' flashed'' step1
      in if null queue then (flashes + length flashed'', zeroed) else go''
  in go

part01 :: [Int] -> Int
part01 xs = flashes
  where
    (flashes, _) = foldl' (const . step) (0, xs) [1..100]

part02 :: [Int] -> Int
part02 xs = fromLeft 0 flash
  where
    size = length xs
    flash = foldlM (\s@(flashes, _) n -> let s'@(flashes', _) = step s in if flashes' - flashes == size then Left n else Right s') (0, xs) [1..]
    

day11 :: IO ()
day11 = do
  input' <- input
  putStrLn "Day 11"
  putStrLn "Part 1"
  putStrLn $ show $ part01 input'
  putStrLn "Part 2"
  putStrLn $ show $ part02 input'