module AdventOfCode.Day2 where

day2 :: IO ()
day2 = do
  input <- readFile "src/Data/Day2.txt"
  putStr "Day 2 part 1: "
  print . part1 $ input
  putStr "Day 2 part 2: "
  print . part2 $ input

part1 :: String -> Int
part1 input = sum gameScore + sum resultScore'
  where
    games = parseInput input
    gameScore = map (rockPaperScissorsScore . snd) games
    resultScore' = map (resultScore . uncurry play) games

part2 :: String -> Int
part2 input = sum gameScores + sum resultScores'
  where
    games = parseInput2 input
    resultScores' = map (resultScore . snd) games
    gameScores = map (rockPaperScissorsScore . uncurry getMove) games

data RockPaperScissors = Rock | Paper | Scissors
  deriving (Eq, Show)

data Result = Win | Lose | Draw

play :: RockPaperScissors -> RockPaperScissors -> Result
play p1 p2
  | p1 == p2 = Draw
  | otherwise = case (p1, p2) of
      (Rock, Scissors) -> Lose
      (Paper, Rock) -> Lose
      (Scissors, Paper) -> Lose
      _ -> Win

getMove :: RockPaperScissors -> Result -> RockPaperScissors
getMove p1 Draw = p1
getMove p1 Lose
  | p1 == Rock = Scissors
  | p1 == Paper = Rock
  | p1 == Scissors = Paper
getMove p1 Win
  | p1 == Rock = Paper
  | p1 == Paper = Scissors
  | p1 == Scissors = Rock

rockPaperScissorsScore :: RockPaperScissors -> Int
rockPaperScissorsScore Rock = 1
rockPaperScissorsScore Paper = 2
rockPaperScissorsScore Scissors = 3

resultScore :: Result -> Int
resultScore Win = 6
resultScore Lose = 0
resultScore Draw = 3

parseLine :: String -> (RockPaperScissors, RockPaperScissors)
parseLine [p1, _, p2] = (parseP1 p1, parseP2 p2)
  where
    parseP1 'A' = Rock
    parseP1 'B' = Paper
    parseP1 'C' = Scissors
    parseP1 _ = error "Invalid input!"
    parseP2 'X' = Rock
    parseP2 'Y' = Paper
    parseP2 'Z' = Scissors
    parseP2 _ = error "Invalid input!"
parseLine _ = error "Invalid input!"

parseInput :: String -> [(RockPaperScissors, RockPaperScissors)]
parseInput = map parseLine . filter (/= "") . lines

parseLine2 :: String -> (RockPaperScissors, Result)
parseLine2 [p1, _, r1] = (parseP1 p1, parseR1 r1)
  where
    parseP1 'A' = Rock
    parseP1 'B' = Paper
    parseP1 'C' = Scissors
    parseP1 _ = error "Invalid input!"
    parseR1 'X' = Lose
    parseR1 'Y' = Draw
    parseR1 'Z' = Win
    parseR1 _ = error "Invalid input!"
parseLine2 _ = error "Invalid input!"

parseInput2 :: String -> [(RockPaperScissors, Result)]
parseInput2 = map parseLine2 . filter (/= "") . lines
