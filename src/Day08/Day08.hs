{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Day08.Day08 where



import Data.List (sort, sortOn, nub, elemIndex)
import Data.Set (Set, fromList, difference, size, toList, intersection)
import Combinatorics (permute)
import Data.Maybe (fromJust)

newtype Input = Input ([String], [String]) deriving (Show)
data Segment = A | B | C | D | E | F | G deriving (Show, Eq, Enum, Read)

parse :: String -> Input
parse line = Input (fst ls, tail $ snd ls)
  where
    ls = span (/="|") $ words line

part01 :: [Input] -> Int
part01 is = length $ concat $ map go is
  where
    go (Input (_, rs)) = filter (`elem` [2, 3, 4, 7]) $ map length rs


--part02' :: Input -> Int
part02' (Input (ls, rs)) = foldl (\acc x -> acc * 10 + x) 0 $ map (fromJust . (`elemIndex` digits) . fromList) rs
  where
    merged = map fromList $ ls ++ rs
    byLength' = byLength merged
    n1 = head $ byLength' 2
    n4 = head $ byLength' 4
    n7 = head $ byLength' 3
    n8 = head $ byLength' 7
    s6 = byLength' 6
    s5 = byLength' 5
    n9 = head $ filter ((==4) . length . (n4 `intersection`)) $ s6
    s6m9 = filter (/=n9) s6
    [n6, n0] = sortOn (length . (n1 `intersection`)) $ s6m9
    n3 = head $ filter ((==2) . length . (n1 `intersection`)) $ s5
    s5m3 = filter (/=n3) $ s5
    [n5, n2] = sortOn (length . (`difference` n6)) s5m3
    digits = [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]


byLength :: [Set Char] -> Int -> [Set Char]
byLength xs n = nub $ sort $ filter ((==n) . length) xs

--part02 :: [Input] -> Int
part02 is = sum $ map part02' is

input :: IO [Input]
input = do
  contents <- readFile "input/Day08.txt"
  return $ map parse $ lines contents

day08 :: IO ()
day08 = do
  input' <- input
  putStrLn $ show $ part01 input'
  putStrLn $ show $ part02 input'
  return ()
