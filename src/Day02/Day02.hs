module Day02.Day02 where

import Data.List
import Data.Maybe
import Data.Bifunctor (bimap)

type Movement = (Int, Int)

move1 :: [String] -> Movement
move1 = foldr1 (\x y -> bimap (fst x +) (snd x +) y) . map parse

--move2 :: [String] -> Movement
move2 = foldl1 (\x y ->
    let a = fst x + fst y
        b = snd x + (snd y * if snd y == 0 then 0 else fst x)
    in (a, b)
  ) . map parse

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
  input <- take 3 . lines <$> readFile "input/day02.txt"
  print $ uncurry (*) (move1 input)
  print $ map parse input
  print $ move2 input
  --print $ uncurry (*) (move2 input)
