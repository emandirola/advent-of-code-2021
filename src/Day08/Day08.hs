module Day08.Day08 where


newtype Input = Input ([String], [String]) deriving (Show)
data Segment = A | B | C | D | E | F | G deriving (Show, Eq, Enum, Read)
data Display = Display { a, b, c, d, e, f, g :: Segment } deriving (Show)

parse :: String -> Input
parse line = Input (fst ls, tail $ snd ls)
  where
    ls = span (/="|") $ words line

part01 :: [Input] -> Int
part01 is = length $ concat $ map go is
  where
    go (Input (_, rs)) = filter (`elem` [2, 3, 4, 7]) $ map length rs

input :: IO [Input]
input = do
  contents <- readFile "input/Day08.txt"
  return $ map parse $ lines contents

day08 :: IO ()
day08 = do
  input' <- input
  putStrLn $ show $ part01 input'
  return ()
