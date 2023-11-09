{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day8 where

import Data.Char (digitToInt)
import Data.List (transpose)
import Lens.Micro (over, _1)

type TreeLevelLine = [(Int, Bool)]

type TreeLevelMap = [TreeLevelLine]

day8 :: IO ()
day8 = do
  input <- readFile "src/Data/Day8.txt"
  putStr "Day 8 part1: "
  print . part1 $ input
  putStr "Day 8 part2: "
  print . part2 $ input

part1 :: String -> Int
part1 = countVisible . checkVisibilityR . checkVisibilityL . transpose . checkVisibilityR . checkVisibilityL . initFlags . inputParser

test :: IO ()
test = print "Hello World"

part2 :: String -> Int
part2 input = maximum . map (scenicScore tm) $ grid
  where
    tm = inputParser input
    yDim = length tm - 1
    xDim = length (head tm) - 1
    grid = [(x, y) | x <- [0 .. xDim], y <- [0 .. yDim]]

inputParser :: String -> [[Int]]
inputParser = map (map digitToInt) . init . lines

initFlags :: [[Int]] -> TreeLevelMap
initFlags = map (map (,False))

checkVisibilityLineR :: TreeLevelLine -> TreeLevelLine
checkVisibilityLineR = map fst . tail . reverse . scanr scanFunc ((-1, False), -1)

checkVisibilityR :: TreeLevelMap -> TreeLevelMap
checkVisibilityR = map checkVisibilityLineR

checkVisibilityLineL :: TreeLevelLine -> TreeLevelLine
checkVisibilityLineL = map fst . tail . scanl (flip scanFunc) ((-1, False), -1)

checkVisibilityL :: TreeLevelMap -> TreeLevelMap
checkVisibilityL = map checkVisibilityLineL

scanFunc :: (Int, Bool) -> ((Int, Bool), Int) -> ((Int, Bool), Int)
scanFunc (tl, vis) (_, maxTl) = ((tl, vis || tl > maxTl), max tl maxTl)

countVisible :: TreeLevelMap -> Int
countVisible = sum . map (length . filter snd)

viewingDistance :: [Int] -> Int -> Int
viewingDistance [] _ = 0
viewingDistance [_] _ = 1
viewingDistance trees hight = visTreeCount + restCount
  where
    (visTrees, rest) = span (< hight) trees
    visTreeCount = length visTrees
    restCount = if rest == [] then 0 else 1

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore treeMap (x, y)
  | treeH == treeV = viewingDistance left treeH * viewingDistance right treeH * viewingDistance up treeV * viewingDistance down treeV
  | otherwise = error "Somethign went wrong!"
  where
    (left, treeH : right) = over _1 reverse . splitAt x $ (treeMap !! y)
    (up, treeV : down) = over _1 reverse . splitAt y $ (transpose treeMap !! x)
