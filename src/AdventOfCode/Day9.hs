module AdventOfCode.Day9 where

import Data.Map (Map)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, parse, try, (<|>))
import Text.Megaparsec.Char (char, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (Left, Right)
import qualified Prelude (Either (Left, Right))

type Parser = Parsec Void String

data RopeST = Rope
  { _head :: (Int, Int),
    _tail :: (Int, Int),
    _tailTrail :: Map (Int, Int) Int
  }
  deriving (Show)

data Motion
  = Up {_n :: Int}
  | Down {_n :: Int}
  | Left {_n :: Int}
  | Right {_n :: Int}
  deriving (Show, Eq)

day9 :: IO ()
day9 = do
  input <- readFile "src/Data/Day9.txt"
  putStr "Day 9 part 1: "
  print . part1 $ input
  putStr "Day 9 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = -1
  where
    motions = case parse inputParser "" input of
      (Prelude.Right result) -> result
      (Prelude.Left _) -> error "Invalid input!"

part2 :: String -> Int
part2 input = -1
  where
    motions = case parse inputParser "" input of
      (Prelude.Right result) -> result
      (Prelude.Left _) -> error "Invalid input!"

motionParser :: Char -> (Int -> Motion) -> Parser Motion
motionParser mc m = m <$> (char mc *> space *> decimal <* eol)

upParser :: Parser Motion
upParser = motionParser 'U' Up

downParser :: Parser Motion
downParser = motionParser 'D' Down

leftParser :: Parser Motion
leftParser = motionParser 'L' Left

rightParser :: Parser Motion
rightParser = motionParser 'R' Right

inputParser :: Parser [Motion]
inputParser = many (try upParser <|> try downParser <|> try leftParser <|> try rightParser) <* eol <* eof

adjustTailPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
adjustTailPos (hx, hy) (tx, ty)
  | abs dx <= 1 && abs dy <= 1 = (tx, ty)
  | otherwise = (tx + signum dx, ty + signum dy)
  where
    dx = hx - tx
    dy = hy - ty

expandMotion :: Motion -> [Motion]
expandMotion m = replicate (_n m) m {_n = 1}
