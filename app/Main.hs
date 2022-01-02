module Main where

import Day01.Day01
import Day02.Day02
import Day03.Day03
import Day04.Day04
import Day05.Day05
import Day06.Day06
import Day07.Day07
import Day08.Day08
import Day09.Day09
import Day10.Day10
import Day11.Day11
import Day12.Day12
import Day13.Day13
import Day14.Day14
import System.IO (hFlush, stdout)
import System.TimeIt (timeItNamed)
  
main :: IO ()
main = do
  putStr "Day: "
  hFlush stdout
  day <- readLn
  _ <- timeItNamed ("Time Day " ++ show day) $ [
    day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12,
    day13, day14
    ] !! (day - 1)
  putStrLn "Hello World"