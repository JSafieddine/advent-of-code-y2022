module AdventOfCode.Day7 where

import Data.Map (Map, empty, filter, foldr, insertWith, toList, (!))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, eof, parse, some, (<|>))
import Text.Megaparsec.Char (char, eol, letterChar, printChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

day7 :: IO ()
day7 = do
  input <- readFile "src/Data/Day7.txt"
  putStr "Day 7 part 1: "
  print . part1 $ input
  putStr "Day 7 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = Data.Map.foldr (+) 0 . Data.Map.filter (<= 100000) . scanDirs $ cmds
  where
    cmds = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

part2 :: String -> Int
part2 input = getSmallestDeletabelDirSize . scanDirs $ cmds
  where
    cmds = case parse inputParser "" input of
      (Right result) -> result
      (Left _) -> error "Invalid input!"

data Command
  = CD {_dir :: String}
  | LS {_content :: [Content]}
  deriving (Show, Eq)

data Content
  = File {_size :: Int, _name :: String}
  | Dir {_name :: String}
  deriving (Show, Eq)

cdParser :: Parser Command
cdParser = CD <$> (string "$ cd " *> try (string "/" <|> string ".." <|> some letterChar) <* eol)

contentParser :: Parser Content
contentParser = (Dir <$> (string "dir " *> some letterChar) <|> (File <$> decimal <* char ' ' <*> some printChar)) <* eol

lsParser :: Parser Command
lsParser = LS <$> (string "$ ls" *> eol *> some contentParser)

inputParser :: Parser [Command]
inputParser = some (cdParser <|> lsParser) <* eol <* eof

scanDir :: (Map String Int, [String]) -> Command -> (Map String Int, [String])
scanDir (dirMap, _) (CD "/") = (dirMap, ["/"])
scanDir (dirMap, parents) (CD "..") = (dirMap, tail parents)
scanDir (dirMap, parents) (CD dir) = (dirMap, (head parents ++ '/' : dir) : parents)
scanDir (dirMap, parents) (LS content) = (foldl addSize dirMap parents, parents)
  where
    files = Prelude.filter isFile content
    size = sum . map _size $ files
    addSize dMap dir = insertWith (+) dir size dMap

scanDirs :: [Command] -> Map String Int
scanDirs = fst . foldl scanDir (empty, [])

isFile :: Content -> Bool
isFile (File _ _) = True
isFile (Dir _) = False

getSmallestDeletabelDirSize :: Map String Int -> Int
getSmallestDeletabelDirSize dirs = minimum . map snd . toList $ Data.Map.filter (\dirSize -> requiredFreeSpace < freeSpace + dirSize) dirs
  where
    totalSpace = 70000000
    requiredFreeSpace = 30000000
    totalUsedSpace = dirs ! "/"
    freeSpace = totalSpace - totalUsedSpace
