{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Day09.Day09 where

import Data.List (sort, nub)
import System.TimeIt (timeItNamed)

type Board = [Cell]
type Position = (Int, Int)
type Positions = [Position]
type Cells = [Cell]
data Cell = Cell Position Int Positions deriving (Show)
instance Eq Cell where
  (Cell p1 _ _) == (Cell p2 _ _) = p1 == p2

input :: IO [[Int]]
input = map (map ((subtract (fromEnum '0')) . fromEnum)) . lines <$> readFile "input/Day09.txt"

buildBoard :: [[Int]] -> Board
buildBoard xss = board
  where
    positions = [(x, y) | y <- [0..length xss - 1], x <- [0..length (head xss) - 1]]
    numbered = zip positions $ concat xss
    board = map (
        \(p, n) -> Cell p n (uncurry adj p)
      ) numbered
    adj = adjacent (length $ head xss) (length xss)

buildBoard' :: [[Int]] -> [Int]
buildBoard' = concat

part01' :: [[Int]] -> Int
part01' !xs = go
  where
    sizex = length $ head xs
    sizey = length xs
    go = go' [0..sizex * sizey - 1] [] [] $ concat xs
    go' !queue !checked !lower !ys =
      if null queue then sum $ map (+1) $ map (ys!!) lower
      else let
        next = head queue
        next' = ys !! next
        neighbors' = neighbors sizex sizey next
        isLower = all (> next') $ map (ys !!) neighbors'
        lower' = if isLower then next:lower else lower
        checked' = next:checked
        queue' = tail queue
      in go' queue' checked' lower' ys


neighbors :: Int -> Int -> Int -> [Int]
neighbors sizex sizey i = map (\(x', y') -> x' + sizex * y') $
  filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < sizex && y' < sizey)
  [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    where
      (x, y) = (i `mod` sizex, i `div` sizex)

lookupCell :: Board -> Position -> Cell
lookupCell board pos = filter (\(Cell p _ _) -> p == pos) board !! 0

adjacent :: Int -> Int -> Int -> Int -> [Position]
adjacent maxx maxy x y = filter (\(x', y') -> x' >= 0 && x' < maxx && y' >= 0 && y' < maxy) [
    (x, y - 1),
    (x, y + 1),
    (x - 1, y),
    (x + 1, y)
  ]

lowPoints :: [Cell] -> [Cell]
lowPoints board = filter (\(Cell _ n cs) -> all (\(Cell _ n' _) -> n < n') $ map lookupCell' cs) board
  where
    lookupCell' = lookupCell board

part01 :: Board -> Int
part01 board = sum $ map ((+1) . (\(Cell _ n _) -> n)) $ lowPoints board

part02 :: Board -> Int
part02 board = product $ take 3 $ reverse $ sort $ map (length . map ((\(Cell _ p _) -> p) . lookupCell') . \p -> basin [p] []) lp
  where
    lookupCell' = lookupCell board
    lp = map (\(Cell p _ _) -> p) $ lowPoints board
    basin queue visited =
      if null queue
      then visited
      else
        let
          next = head queue
          (Cell p _ cs) = lookupCell' next
          adjs = map lookupCell' cs
          nexts = filter (\p' -> p' `notElem` visited) $ filter (\p' -> p' `notElem` queue) $ map (\(Cell p' _ _) -> p') $ filter (\(Cell _ n _) -> n /= 9) adjs
        in
          basin (tail queue ++ nexts) (visited ++ [p])

day09 :: IO ()
day09 = do
  board <- input
  putStrLn "Day 09"
  board `seq` putStrLn "Board"
  putStrLn "Part 01"
  timeItNamed "part 01" $ putStrLn $ show $ part01' board
  --timeItNamed "part 01" $ putStrLn $ show $ part01 $ buildBoard board
  putStrLn "Part 02"
  --timeItNamed "part 02" $ putStrLn $ show $ part02 board