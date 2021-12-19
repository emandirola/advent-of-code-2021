module Day10.Day10 where

import Data.Text (Text, replace, pack, unpack, findIndex)
import qualified Data.Text as T (length)
import Data.Either (fromLeft, lefts)
import Data.Maybe (fromJust, isJust)
type Parser = (String, Maybe Char)

chunks :: [Text]
chunks = map pack ["()", "[]", "{}", "<>"]
points :: Char -> Int
points c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> 0


parse :: String -> Maybe Char
parse s = fmap (\i -> unpack parsed !! i) $ findIndex (`elem` ending) $ parsed
  where
    parsed = fromLeft (pack "") $ parse' $ pack s
    ending = map ((!! 1) . unpack) chunks
    replaceAll = foldl1 (.) $ map (`replace` pack "") chunks
    parse' :: Text -> Either Text Text
    parse' s' =
      let
        replaced = replaceAll s'
      in if T.length replaced == T.length s'
        then Left replaced
        else parse' replaced

part01 :: [String] -> Int
part01 xs = sum $ map (points . fromJust) $ filter isJust xs'
  where
    xs' = map parse xs

input :: IO [String]
input = lines <$> readFile "input/day10.txt"

day10 :: IO ()
day10 = do
  input' <- input
  putStrLn "Day 10"
  putStrLn "Part 1"
  putStrLn $ show $ part01 input'

