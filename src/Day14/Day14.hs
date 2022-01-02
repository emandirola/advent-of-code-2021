module Day14.Day14 where

import Data.Maybe (fromJust, isJust)
import Data.Foldable (foldl', find)
import Data.List (iterate', sort, group)


type Template = String
type Rule = ((Char, Char), Char)
type Rules = [Rule]

parseInput :: [String] -> (Template, Rules)
parseInput input = (template, rules)
  where
    (template:_:rs) = input
    rules = map parseRule rs
    parseRule s = (toTuple $ take 2 s, s !! 6)
    toTuple (x:y:_) = (x, y)

part01 :: Template -> Rules -> Int
part01 ts rs = last bySize - head bySize
  where
    bySize = sort $ map length $ group $ sort res
    res = map fst $ iterate' replace ts' !! 10
    ts' = zip (ts ++ " ") (tail ts ++ " ")
    replace :: [(Char, Char)] -> [(Char, Char)]
    replace template = foldl' (\acc t@(a, b) -> acc ++ let r = t `lookup` rs in if isJust r then [(a, fromJust r), (fromJust r, b)] else [t]) [] template

day14 :: IO ()
day14 = do
  (template, rules) <- parseInput . lines <$> readFile "input/day14.txt"
  putStrLn "Day 14"
  putStrLn "Part 1"
  putStrLn $ show $ part01 template rules
  putStrLn "Part 2"