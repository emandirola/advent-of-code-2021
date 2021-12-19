module Day10.Day10 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (fromLeft, partitionEithers)
import Data.Maybe (fromJust, isJust)
import Data.List (sort)
type Parser = (String, Maybe Char)

chunks :: [Text]
chunks = map T.pack ["()", "[]", "{}", "<>"]
ending :: [Char]
ending = map ((!! 1) . T.unpack) chunks
openingAndEnding :: [(Char, Char)]
openingAndEnding = zip (map ((!! 0) . T.unpack) chunks) ending
openingToEnding o = snd $ head $ filter (\(a, _) -> a == o) openingAndEnding
points :: Char -> ((Int, Int), Char)
points c = case c of
  ')' -> ((3, 1), '(')
  ']' -> ((57, 2), '[')
  '}' -> ((1197, 3), '{')
  '>' -> ((25137, 4), '<')
  _ -> ((0, 0), ' ')


--parse :: String -> Maybe Char
parse :: [Char] -> Text
parse s = parsed
  where
    parsed = fromLeft (T.pack "") $ parse' $ T.pack s
    replaceAll = foldl1 (.) $ map (`T.replace` T.pack "") chunks
    parse' :: Text -> Either Text Text
    parse' s' =
      let
        replaced = replaceAll s'
      in if T.length replaced == T.length s'
        then Left replaced
        else parse' replaced

parseAll :: [String] -> ([Text], [Text])
parseAll xs = partitionEithers xs'
  where
    xs' = map parse' xs
    parse' s = if T.any (`elem` ending) parsed
      then Left parsed
      else Right parsed
      where
        parsed = parse s

part01 :: [String] -> Int
part01 xs = sum $ map (fst . fst . points) lastChars
  where
    parsed = fst $ parseAll xs
    lastChars = map fromJust $ filter isJust $ map (T.find (`elem` ending)) parsed

part02 :: [String] -> Int
part02 xs = parsed !! (length parsed `div` 2)
  where
    parsed = sort $ map (points' . T.unpack) $ snd $ parseAll xs
    points' :: String -> Int
    points' = foldl (\acc p -> acc * 5 + p) 0 . map (snd . fst . points . openingToEnding) . reverse

input :: IO [String]
input = lines <$> readFile "input/day10.txt"

day10 :: IO ()
day10 = do
  input' <- input
  putStrLn "Day 10"
  putStrLn "Part 1"
  putStrLn $ show $ part01 input'
  putStrLn "Part 2"
  putStrLn $ show $ part02 input'

