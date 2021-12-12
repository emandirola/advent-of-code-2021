module Day06.Day06 where

import Data.List (foldl', sort, group)
import Data.List.NonEmpty (groupAllWith)

data Fish a = Fish { days :: Int, amount :: a }
instance Num a => Semigroup (Fish a) where
  (Fish d a1) <> (Fish _ a2) = Fish (d) (a1 + a2)

makeFish :: [Int] -> Fish Int
makeFish xs = Fish (head xs) (length xs)

play :: Int -> [Int] -> Int
play n fish = sum $ map amount $ foldl' go fish' [1..n]
  where
    fish' = map makeFish $ group $ sort fish
    go fs _ = map (foldr1 (<>)) $ groupAllWith days $ concatMap go' fs
    go' (Fish d a) = if d > 0 then [Fish (d-1) a] else [Fish 6 a, Fish 8 a]

part01 :: [Int] -> Int
part01 = play 80

part02 :: [Int] -> Int
part02 = play 256

day06 :: IO ()
day06 = do
  fish <- read <$> (\s -> "[" ++ s ++ "]") <$> readFile "input/day06.txt"
  putStrLn "Day 06"
  putStrLn "Part 1"
  putStrLn $ show $ part01 fish
  putStrLn "Part 2"
  putStrLn $ show $ part02 fish
