{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Day14.Day14 where

import Data.Foldable (foldl')
import Data.List (sort)
import Data.List.NonEmpty (groupAllWith)
import Data.Tuple.Extra (first)


type Template = String
type Rule = ((Char, Char), Char)
type Rules = [Rule]
type PairSize = ((Char, Char), Int)
(!<>) :: PairSize -> PairSize -> PairSize
((a, b), n1) !<> (_, n2) = ((a, b), n1 + n2)

parseInput :: [String] -> (Template, Rules)
parseInput input = (template, rules)
  where
    (template:_:rs) = input
    rules = map parseRule rs
    parseRule s = (toTuple $ take 2 s, s !! 6)
    toTuple (x:y:_) = (x, y)

insert :: Int -> Template -> Rules -> Int
insert n ts rs = last bySize - head bySize
  where
    bySize = sort $ map (foldl1 (+) . fmap snd) $ groupAllWith fst $ map (first fst) res
    res = iterate replace ts' !! n
    ts' = merge $ map ((,1)) $ zip (ts ++ " ") (tail ts ++ " ")
    merge = map (foldr1 (!<>)) . groupAllWith fst
    replace template = merge $ foldl' replace' [] template
    replace' acc ps@((p@(a, b), n')) = acc ++ maybe [ps] (\r' -> [((a, r'), n'), ((r', b), n')]) (p `lookup` rs)

part01 :: Template -> Rules -> Int
part01 = insert 10

part02 :: Template -> Rules -> Int
part02 = insert 40

day14 :: IO ()
day14 = do
  (template, rules) <- parseInput . lines <$> readFile "input/day14.txt"
  putStrLn "Day 14"
  putStrLn "Part 1"
  putStrLn $ show $ part01 template rules
  putStrLn "Part 2"
  putStrLn $ show $ part02 template rules