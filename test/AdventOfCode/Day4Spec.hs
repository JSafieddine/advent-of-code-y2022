{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day4Spec (spec) where

import AdventOfCode.Day4
import Data.Functor ((<&>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "test day 4" $ do
    it "rangeParser test" $
      parse rangeParser "" "123-456" `shouldParse` Range 123 456
    it "lineParser test" $
      parse lineParser "" "123-456,789-1011\n" `shouldParse` (Range 123 456, Range 789 1011)
    it "inputParser test" $ do
      input <- readFile "test/Data/Day4.txt"
      parse inputParser "" input
        `shouldParse` [ (Range 2 4, Range 6 8),
                        (Range 2 3, Range 4 5),
                        (Range 5 7, Range 7 9),
                        (Range 2 8, Range 3 7),
                        (Range 6 6, Range 4 6),
                        (Range 2 6, Range 4 8)
                      ]
    it "part one" $
      (readFile "test/Data/Day4.txt" <&> part1) `shouldReturn` 2
    it "part two" $
      (readFile "test/Data/Day4.txt" <&> part2) `shouldReturn` 4
