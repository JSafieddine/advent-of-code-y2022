{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day9Spec (spec) where

import AdventOfCode.Day9
import Data.Functor ((<&>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)
import Prelude hiding (Left, Right)

spec :: Spec
spec = do
  describe "test day 9" $ do
    it "inputParser test" $ do
      input <- readFile "test/Data/Day9.txt"
      parse inputParser "" input
        `shouldParse` [ Right 4,
                        Up 4,
                        Left 3,
                        Down 1,
                        Right 4,
                        Down 1,
                        Left 5,
                        Right 2
                      ]
    it "part one" $
      (readFile "test/Data/Day9.txt" <&> part1) `shouldReturn` 13
    it "part two" $
      (readFile "test/Data/Day9.txt" <&> part2) `shouldReturn` -1
