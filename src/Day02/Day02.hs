module Day02.Day02 where

import Data.List (elemIndex)
import Data.Maybe

type Movement = (Int, Int)

part1 :: [String] -> Movement
part1 = foldr1 add . map parse
  where
    add (x, y) (dx, dy) = (,) (x + dx) (y + dy)

moveDepth :: (Int, Int, Int) -> Movement -> (Int, Int, Int)
moveDepth (x1, y1, a1) (dx, da) = (x2, y2, a2)
  where
    x2 = x1 + dx
    y2 = if dx == 0 then y1 else y1 + (a2 * dx)
    a2 = a1 + da

part2 :: [String] -> Movement
part2 = finish <$> foldl moveDepth (0, 0, 0) . map parse
  where
    finish (x, y, _) = (x, y)

parse :: String -> Movement
parse action
  | isNothing index = (0, 0)
  | otherwise = case command of
    "forward" -> (n, 0)
    "down" -> (0, n)
    "up" -> (0, -n)
    _ -> (0, 0)
  where
    index = elemIndex ' ' action
    (command, amount) = fromJust $ flip splitAt action <$> index
    n = read amount

day02 :: IO ()
day02 = do
  print "Day 02"
  input <- lines <$> readFile "input/day02.txt"
  print "Part 1"
  print $ uncurry (*) (part1 input)
  print "Part 2"
  print $ uncurry (*) (part2 input)
