{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Day09.Day09 where

import Data.List (intersperse)
import Control.Monad (join)
import Data.Maybe (isJust, fromJust)
import Debug.Trace (trace)

type Board = [Cell]
type Position = (Int, Int)
type Cells = [Cell]
data Cell = Cell Position Int Cells

input :: IO [[Int]]
input = map (map ((subtract (fromEnum '0')) . fromEnum)) . lines <$> readFile "input/Day09.example.txt"

buildBoard :: [[Int]] -> Board
buildBoard xss = board
  where
    numbered = zip [0..] $ map (zip [0..]) xss
    preboard = foldl (
      \acc (y, xs) -> acc ++ map (
        \(x, n) -> ((x, y), n)
        ) xs
      ) [] numbered
    board = map (\((x, y), n) -> Cell (x, y) n (map lookupCell' $ adj x y)) preboard
    lookupCell' = lookupCell board
    adj = adjacent (length $ head xss) (length xss)



lookupCell :: Board -> Position -> Cell
lookupCell board pos = filter (\(Cell p _ _) -> p == pos) board !! 0

adjacent :: Int -> Int -> Int -> Int -> [Position]
adjacent maxx maxy x y = filter (\(x', y') -> x' >= 0 && x' <= maxx && y' >= 0 && y' <= maxy) [
    (x, y - 1),
    (x, y + 1),
    (x - 1, y),
    (x + 1, y)
  ]
--part01 :: [[Int]] -> Int
part01 (xss :: [[Int]]) = sum $ map (+1) $ concat xss''
  where
    xss' :: [(Int, [(Int, Int)])]
    xss' = zip [0..] $ map (zip [0..]) xss
    xss'' = map (\(i, xs) -> map snd $ filter (\(j, x) -> cell x i j) xs) xss'
    cell x i j = all ((>x) . fromJust) $ filter isJust [up, down, left, right]
      where
        up = join $ (j `lookup`) <$> ((i - 1) `lookup` xss')
        down = join $ (j `lookup`) <$> ((i + 1) `lookup` xss')
        left = join $ ((j - 1) `lookup`) <$> (i `lookup` xss')
        right = join $ ((j +  1) `lookup`) <$> (i `lookup` xss')


day09 :: IO ()
day09 = do
  input' <- input
  putStrLn $ show $ part01 input'