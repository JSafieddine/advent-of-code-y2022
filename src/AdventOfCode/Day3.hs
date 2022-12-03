module AdventOfCode.Day3 where

import Data.Char (isLower, isUpper, ord)
import Data.List (intersect, nub)
import Data.List.Split (chunksOf)

day3 :: IO ()
day3 = do
  input <- readFile "src/Data/Day3.txt"
  putStr "Day 3 Part 1: "
  print . part1 $ input
  putStr "Day 3 Part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = sum . map calcPriority . filter (/= "") . lines

part2 :: String -> Int
part2 = sum . map calcBadgePriority . chunksOf 3 . filter (/= "") . lines

priority :: Char -> Int
priority c
  | isUpper c = ord c - ord 'A' + 27
  | isLower c = ord c - ord 'a' + 1
  | otherwise = error "Invalid input!"

calcPriority :: String -> Int
calcPriority line = priority common
  where
    half = length line `div` 2
    (c1, c2) = splitAt half line
    common = case nub c1 `intersect` nub c2 of
      [c] -> c
      _ -> error "Invalid input!"

calcBadgePriority :: [String] -> Int
calcBadgePriority [bag1, bag2, bag3] = case nub bag1 `intersect` nub bag2 `intersect` nub bag3 of
  [badge] -> priority badge
  _ -> error "Invalid input!"
calcBadgePriority _ = error "Invalid input!"
