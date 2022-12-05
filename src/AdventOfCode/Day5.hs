module AdventOfCode.Day5 where

import Data.Functor (($>))
import Data.List (transpose)
import Data.Map (Map, adjust, fromAscList, (!))
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, parse, sepBy, some, someTill_, try, (<|>))
import Text.Megaparsec.Char (char, eol, newline, string, upperChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

day5 :: IO ()
day5 = do
  input <- readFile "src/Data/Day5.txt"
  putStr "Day 5 part 1: "
  print . part1 $ input
  putStr "Day 5 part 2: "
  print . part2 $ input

part1 :: String -> String
part1 input = getTopCrates . foldl (moveCrate reverse) cs $ mvs
  where
    (cs, mvs) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

part2 :: String -> String
part2 input = getTopCrates . foldl (moveCrate id) cs $ mvs
  where
    (cs, mvs) = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

crateParser :: Parser (Maybe Char)
crateParser = try (Just <$> (char '[' *> upperChar <* char ']')) <|> (string "   " $> Nothing)

crateLineParser :: Parser [Maybe Char]
crateLineParser = crateParser `sepBy` char ' ' <* eol

stackNumberParser :: Parser Int
stackNumberParser = char ' ' *> decimal <* char ' '

stackNumberLineParser :: Parser [Int]
stackNumberLineParser = stackNumberParser `sepBy` char ' ' <* eol

type CrateStack = Map Int String

crateStackParser :: Parser CrateStack
crateStackParser = do
  (crates, stacks) <- crateLineParser `someTill_` stackNumberLineParser
  return . fromAscList . zip stacks . map catMaybes . transpose $ crates

data Movement = Move {_quantity :: Int, _from :: Int, _to :: Int}
  deriving (Show, Eq)

movementParser :: Parser Movement
movementParser = Move <$> (string "move " *> decimal) <*> (string " from " *> decimal) <*> (string " to " *> decimal) <* eol

inputParser :: Parser (CrateStack, [Movement])
inputParser = (,) <$> crateStackParser <* newline <*> some movementParser <* eol <* eof

moveCrate :: (String -> String) -> CrateStack -> Movement -> CrateStack
moveCrate craneModel cs mv = addCrates craneModel mv crates . removeCrates mv $ cs
  where
    crates = getCrates mv cs

getCrates :: Movement -> CrateStack -> String
getCrates mv cs = take (_quantity mv) (cs ! _from mv)

removeCrates :: Movement -> CrateStack -> CrateStack
removeCrates mv = adjust (drop . _quantity $ mv) (_from mv)

addCrate :: Movement -> Char -> CrateStack -> CrateStack
addCrate mv crate = adjust (crate :) (_to mv)

-- crates need to be added in reverse order
addCrates :: (String -> String) -> Movement -> String -> CrateStack -> CrateStack
addCrates craneModel mv crates cs = foldr (addCrate mv) cs . craneModel $ crates

getTopCrates :: CrateStack -> String
getTopCrates = foldr foldFunc ""
  where
    foldFunc [] result = result
    foldFunc stack result = head stack : result
