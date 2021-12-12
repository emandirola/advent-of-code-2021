{-# LANGUAGE BangPatterns #-}

module Day04.Day04 where

import Data.List (transpose, sort)
import Data.Foldable (foldlM)
import Data.Maybe (maybeToList)

newtype Board = Board (Mark, [[(Bool, Int)]])
newtype Mark = Mark (Position, Int) deriving (Eq, Ord, Show)
newtype Position = Position Int deriving (Eq, Ord, Show)
newtype Score = Score Int deriving (Eq, Ord, Show)
newtype SolvedBoard = SolvedBoard (Position, Score) deriving (Eq, Ord, Show)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f xs =
  let (first, rest) = break f xs
  in first : splitBy f (dropWhile f rest)

buildBoard :: [String] -> Board
buildBoard xs = Board (Mark (Position (-1), -1), board)
  where
    board = map (map (((,) False) . read) . splitBy (== ' ') . dropWhile (== ' ')) xs :: [[(Bool, Int)]]

markBoard :: Board -> Mark -> Board
markBoard (Board (_, board)) m@(Mark (_, n)) = Board (m, map (map mark) board)
  where
    mark q@(_, b)
      | b == n = (True, b)
      | otherwise = q

checkBoard :: Board -> Bool
checkBoard (Board (_, board)) = any (all fst) (board ++ transpose board)

scoreBoard :: Board -> Either SolvedBoard Board
scoreBoard b@(Board ((Mark (p, n)), board)) = if checkBoard b
  then Left $ SolvedBoard (p, Score $ n * (sum . map snd . filter (not . fst) . concat) board)
  else Right b

printBoard :: Board -> String
printBoard (Board (n, board)) = "start board" ++ show n ++ "\n" ++ (unlines . map (unwords . map show)) board ++ "\n" ++ "end board"

play :: [Mark] -> Board -> Maybe SolvedBoard
play marks board = either Just (const Nothing) $ foldlM go board marks
  where
    go b m = scoreBoard $ markBoard b m

playAll :: [Board] -> [Mark] -> [SolvedBoard]
playAll !boards marks = sort $ maybeToList . play marks =<< boards

part01 :: [Board] -> [Mark] -> SolvedBoard
part01 !boards = head . playAll boards

part02 :: [Board] -> [Mark] -> SolvedBoard
part02 !boards = head . reverse . playAll boards 

day04 :: IO ()
day04 = do
  (drawnStr:_:boardsStr) <- lines <$> readFile "input/day04.txt"
  let drawn = map Mark $ zip (map Position [0..]) $ map read $ splitBy (==',') drawnStr :: [Mark]
  let boards = map buildBoard $ takeWhile (not . null) $ map (take 5 . (`drop` boardsStr)) $ [0, 6..]
  putStrLn "Day 04"
  putStrLn "Part 1"
  putStrLn $ show $ part01 boards drawn
  putStrLn "Part 2"
  putStrLn $ show $ part02 boards drawn
  return ()