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
import System.IO (hFlush, stdout)
  
main :: IO ()
main = do
  putStr "Day: "
  hFlush stdout
  day <- readLn
  _ <- [day01, day02, day03, day04, day05, day06, day07, day08, day09, day10] !! (day - 1)
  putStrLn "Hello World"