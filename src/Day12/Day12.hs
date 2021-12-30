{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Day12.Day12 where
  
import Data.Tuple (swap)
import GHC.OldList (nub, find)
import GHC.Unicode (isUpper)
import Data.Maybe (fromJust, mapMaybe, isNothing, isJust)

type Path = [String]
type Pair = (String, String)
type Node = (String, [String])

makePairs :: [String] -> [Pair]
makePairs xs = ps ++ map swap ps
  where
    ps = map (fmap tail . break (=='-')) xs

makeGraph :: [Pair] -> [Node]
makeGraph pairs = nodes
  where
    caves = nub $ map fst pairs
    nodes = map (\c -> (c, children c)) caves
    children c = map snd $ filter ((==c) . fst) pairs

allPaths01 :: [Node] -> [Path]
allPaths01 nodes = go [] [] start
  where
    findNode n = find ((==n) . fst) nodes
    start = fromJust $ findNode "start"
    go visited path n =
      let
          name = fst n
          children = mapMaybe findNode $ filter (`notElem` visited) $ snd n
          visited' = if isUpper $ head name then visited else name : visited
          path' = name:path
          bfs = go visited' path' `concatMap` children
      in
         if
           | name == "end" -> [path']
           | null children -> []
           | otherwise -> bfs

allPaths02 :: [Node] -> [Path]
allPaths02 nodes = go ["start"] [] [] Nothing start
  where
    findNode n = find ((==n) . fst) nodes
    start = fromJust $ findNode "start"
    go visited path lowers single n =
      let
        name = fst n
        single' = if isNothing single then find (==name) lowers else single
        children = mapMaybe findNode $ filter (`notElem` visited') $ snd n
        lowers' = if isUpper $ head name then lowers else name : lowers
        visited' = if
                     | isUpper $ head name -> visited
                     | isNothing single' -> visited
                     | isNothing single && isJust single' -> name : lowers'
                     | otherwise -> name : visited
        path' = name:path
        bfs = go visited' path' lowers' single' `concatMap` children
      in
        if
          | name == "end" -> [path']
          | null children -> []
          | otherwise -> bfs

part01 :: [String] -> Int
part01 is = length $ allPaths01 $ makeGraph $ makePairs is

part02 :: [String] -> [Path]
part02 is = allPaths02 $ makeGraph $ makePairs is

day12 :: IO ()
day12 = do
  !input <- lines <$> readFile "input/day12.txt"
  let !g = makeGraph $ makePairs input
  putStrLn $ unlines $ map show g
  putStrLn "Day 12"
  putStrLn "Part 1"
  --putStrLn $ show $ part01 input
  putStrLn "Part 2"
  --putStrLn $ unlines $ map unwords $ part02 input
  --putStrLn $ show $ length $ part02 input
