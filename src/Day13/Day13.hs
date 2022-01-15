module Day13.Day13 where

import Data.Tuple.Extra (both, first)
import Data.List (sort, nub, foldl')

type Dots = [(Int, Int)] -- x, y
type Folds = [(Int, Int)] -- x, y

process :: [String] -> (Dots, Folds)
process xs = (dots, folds)
  where
    (ds, fs) = tail <$> break (== "") xs
    dots = map (both read . fmap tail . break (==',')) ds
    folds :: Folds
    folds = map ((\(a, b) -> if a == "fs" then (read b, 0) else (0, read b)) . (fmap tail . break (== '=') . drop 11)) fs

foldDots :: Int -> Int -> Dots -> Folds -> (Dots, (Int, Int))
foldDots maxX maxY ds fs = first (nub . sort) $ foldl' go (ds, (maxX, maxY)) fs
  where
    sub (x, y) (dx, dy) = (x - dx, y - dy)
    go :: (Dots, (Int, Int)) -> (Int, Int) -> (Dots, (Int, Int))
    go (ds, (maxX, maxY)) f = ds'
      where
        ds' = (foldxy ds f, if fst f == 0 then (maxX, snd f - 1) else (fst f - 1, maxY))
    foldxy acc f = map (foldxy' f) acc
    foldxy' (fx, fy) p@(x, y) = if xy > d then sub p dxy else p
      where
        (xy, d, dxy) = if fx > 0
          then (x, fx, (2 * (x - fx), 0))
          else (y, fy, (0, 2 * (y - fy)))

showDots :: (Dots, (Int, Int)) -> String
showDots (ds, (maxx, maxy)) = m
  where
    padRight s = replicate (2 - length s) ' ' ++ s
    m = unlines [concatMap padRight [if (x, y) `elem` ds then "#" else "." | x <- [0..maxx]] | y <- [0..maxy]]

part01 :: Dots -> Folds -> Int
part01 ds fs = length $ fst $ foldDots maxX maxY ds [head fs]
  where
    maxX = maximum $ map fst ds
    maxY = maximum $ map snd ds

part02 :: Dots -> Folds -> String
part02 ds fs = m
  where
    maxX = maximum $ map fst ds
    maxY = maximum $ map snd ds
    foldedDots = foldDots maxX maxY ds fs
    m = showDots foldedDots

day13 :: IO ()
day13 = do
  (dots, folds) <- process . lines <$> readFile "input/day13.txt"
  putStrLn "Day 13"
  putStrLn "Part 01"
  print (part01 dots folds)
  putStrLn "Part 02"
  putStrLn $ part02 dots folds
  