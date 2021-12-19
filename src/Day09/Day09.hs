{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Day09.Day09 where

type Board = [Cell]
type Position = (Int, Int)
type Positions = [Position]
type Cells = [Cell]
data Cell = Cell Position Int Positions deriving (Show, Eq)

input :: IO [[Int]]
input = map (map ((subtract (fromEnum '0')) . fromEnum)) . lines <$> readFile "input/Day09.example.txt"

buildBoard :: [[Int]] -> Board
buildBoard xss = board
  where
    positions = [(x, y) | y <- [0..length xss - 1], x <- [0..length (head xss) - 1]]
    numbered = zip positions $ concat xss
    board = map (
        \(p, n) -> Cell p n (uncurry adj p)
      ) numbered
    adj = adjacent (length $ head xss) (length xss)

lookupCell :: Board -> Position -> Cell
lookupCell board pos = filter (\(Cell p _ _) -> p == pos) board !! 0

adjacent :: Int -> Int -> Int -> Int -> [Position]
adjacent maxx maxy x y = filter (\(x', y') -> x' >= 0 && x' < maxx && y' >= 0 && y' < maxy) [
    (x, y - 1),
    (x, y + 1),
    (x - 1, y),
    (x + 1, y)
  ]

part01 :: Board -> Int
part01 board = sum $ map ((+1) . (\(Cell _ n _) -> n)) $ board'
  where
    lookupCell' = lookupCell board
    board' = filter (\(Cell _ n cs) -> all (\(Cell _ n' _) -> n < n') $ map lookupCell' cs) board

part02 :: Board -> Int
part02 = undefined

day09 :: IO ()
day09 = do
  board <- buildBoard <$> input
  putStrLn $ show $ part01 board