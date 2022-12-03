module AdventOfCode.Day1 (day1, part1, part2) where

import Data.List (sort)
import Data.List.Split (splitOn)

day1 :: IO ()
day1 = do
  input <- readFile "src/Data/Day1.txt"
  putStr "Day1 part1: "
  print . part1 $ input
  putStr "Day 1 part2: "
  print . part2 $ input

parseInput :: String -> [[Int]]
parseInput = map (map read) . filter (/= []) . splitOn [""] . lines

part1 :: String -> Int
part1 = maximum . map sum . parseInput

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map sum . parseInput
