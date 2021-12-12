{-# LANGUAGE BangPatterns #-}

module Day05.Day05 where

import Data.Char (isDigit)
import Data.List (sort, group)


newtype Point = Point ((Int, Int), (Int, Int)) deriving (Show)

parsePoint :: String -> Point
parsePoint s = point
  where
    (f, rest) = break (==' ') s
    (x1, y1) = read ("(" ++ f ++ ")") :: (Int, Int)
    (x2, y2) = read ("(" ++ (dropWhile (not . isDigit) rest) ++ ")") :: (Int, Int)
    point = Point ((x1, y1), (x2, y2))

markedPoints01 :: Point -> [(Int, Int)]
markedPoints01 (Point ((x1, y1), (x2, y2))) = [(x, y) | x1 == x2 || y1 == y2,  x <- range x1 x2, y <- range y1 y2]

markedPoints02 :: Point -> [(Int, Int)]
markedPoints02 p@(Point ((x1, y1), (x2, y2)))
  | x1 == x2 || y1 == y2 = markedPoints01 p
  | otherwise = diagonalPoints
  where
    step a1 a2 = (a1, a2) : step a1' a2'
      where
        a1' = a1 + signum (x2 - x1)
        a2' = a2 + signum (y2 - y1)
    diagonalPoints = [(x, y) | (x, y) <- take (length $ range x1 x2) $ step x1 y1]

range :: Int -> Int -> [Int]
range a b
  | a == b = [a..b]
  | otherwise = [a, a + signum (b - a)..b]

countPoints :: [[(Int, Int)]] -> Int
countPoints = length . filter ((>1) . length) . group . sort . concat

part01:: [Point] -> Int
part01 ps = countPoints $ map markedPoints01 ps

part02:: [Point] -> Int
part02 ps = countPoints $ map markedPoints02 ps

day05 :: IO ()
day05 = do
  points <- map parsePoint <$> lines <$> readFile "input/day05.txt" :: IO [Point]
  putStrLn "Day 05"
  putStrLn "Part 01"
  putStrLn $ show $ part01 points
  putStrLn "Part 02"
  putStrLn $ show $ part02 points
  return ()
