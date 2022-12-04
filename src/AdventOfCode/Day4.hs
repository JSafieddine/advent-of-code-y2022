module AdventOfCode.Day4 where

import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, parse)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

day4 :: IO ()
day4 = do
  input <- readFile "src/Data/Day4.txt"
  putStr "Day 4 Part 1: "
  print . part1 $ input
  putStr "Day 4 Part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = length . filter (== FullyContained) . map compareRange $ ranges
  where
    ranges = case parse inputParser "" input of
      (Right success) -> success
      (Left _) -> error "Invalid input"

part2 :: String -> Int
part2 input = length . filter (/= Disjunct) . map compareRange $ ranges
  where
    ranges = case parse inputParser "" input of
      (Right success) -> success
      (Left _) -> error "Invalid input"

data Range = Range {_start :: Int, _end :: Int}
  deriving (Show, Eq)

rangeParser :: Parser Range
rangeParser = Range <$> (decimal <* char '-') <*> decimal

lineParser :: Parser (Range, Range)
lineParser = (,) <$> (rangeParser <* char ',') <*> rangeParser <* eol

inputParser :: Parser [(Range, Range)]
inputParser = many lineParser <* eol <* eof

data RangeCompare = FullyContained | Intersecting | Disjunct
  deriving (Show, Eq)

compareRange :: (Range, Range) -> RangeCompare
compareRange (a, b)
  | _start a <= _start b && _end a >= _end b = FullyContained
  | _start a >= _start b && _end a <= _end b = FullyContained
  | _end a < _start b || _end b < _start a = Disjunct
  | otherwise = Intersecting
