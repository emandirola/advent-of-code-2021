{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Day12.Day12 where
  
import Data.Tuple (swap)
import GHC.OldList (nub, find)
import GHC.Unicode (isUpper)
import Data.Maybe (fromJust, mapMaybe, isNothing, isJust)
import Data.List (sortBy, sort)
import Debug.Trace (traceShowId, traceShow)

type Path = [Int]
type Pair = (Int, Int)
type Node = (Int, [Int])

makePairs :: [String] -> [Pair]
makePairs xs = ps' ++ map swap ps'
  where
    ps = map (fmap tail . break (=='-')) xs
    cleanPs = filter (\n -> n /= "start" && n/= "end" ) $ nub $ map fst (ps ++ map swap ps)
    nodes = map (\(a, b) -> (a, if isUpper $ head a then b * 1000 else b)) $ zip ("start" : "end" : cleanPs) [0::Int ..]
    ps' = map (\(a, b) -> (fromJust $ lookup a nodes, fromJust $ lookup b nodes)) ps


makeGraph :: [Pair] -> [Node]
makeGraph pairs = sort nodes
  where
    caves = nub $ map fst pairs
    nodes = map (\c -> (c, children c)) caves
    children c = map snd $ filter ((==c) . fst) pairs

allPaths01 :: [Node] -> [Path]
allPaths01 nodes = go [] [] (start)
  where
    findNode n = find (\(n', _) -> n == n') nodes
    start = nodes !! 0
    end = nodes !! 1
    go :: [Int] -> [Int] -> Node -> [Path]
    go visited path n =
      let
          node = fst n
          children = filter (`notElem` visited) $ snd n
          visited' = if node > 1000 then visited else node : visited
          path' = node:path
          !bfs = go visited' path' `concatMap` mapMaybe findNode children
      in
         if
           | n == end -> [path']
           | null children -> []
           | otherwise -> bfs

allPaths02 :: [Node] -> [Path]
allPaths02 nodes = go [fst start] [] [] Nothing start
  where
    findNode n = find (\(n', _) -> n == n') nodes
    start = nodes !! 0
    end = nodes !! 1
    go visited path lowers single n =
      let
        node = fst n
        single' = if isNothing single then find (==node) lowers else single
        children = filter (`notElem` visited') $ snd n
        lowers' = if node > 1000 then lowers else node : lowers
        visited' = if
                     | node > 1000 -> visited
                     | isNothing single' -> visited
                     | isNothing single && isJust single' -> node : lowers'
                     | otherwise -> node : visited
        path' = node:path
        bfs = go visited' path' lowers' single' `concatMap` mapMaybe findNode children
      in
        if
          | n == end -> [path']
          | null children -> []
          | otherwise -> bfs

part01 :: [String] -> [Path]
part01 is = allPaths01 $ makeGraph $ makePairs is

part02 :: [String] -> [Path]
part02 is = allPaths02 $ makeGraph $ makePairs is

day12 :: IO ()
day12 = do
  !input <- lines <$> readFile "input/day12.txt"
  putStrLn "Day 12"
  putStrLn "Part 1"
  putStrLn $ show $ length $ part01 input
  putStrLn "Part 2"
  --putStrLn $ unlines $ map unwords $ part02 input
  putStrLn $ show $ length $ part02 input
