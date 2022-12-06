module AdventOfCode.Day6 where

import Data.List (nub, tails)

day6 :: IO ()
day6 = do
  input <- readFile "src/Data/Day6.txt"
  putStr "Day 6 part 1: "
  print . part1 $ input
  putStr "Day 6 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 = getMarkerPosition 4

part2 :: String -> Int
part2 = getMarkerPosition 14

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

getMarkerPosition :: Int -> String -> Int
getMarkerPosition pos input = marker
  where
    w = windows pos input
    wp = zip [pos ..] w
    (marker, _) = head . dropWhile ((/= pos) . length . nub . snd) $ wp
