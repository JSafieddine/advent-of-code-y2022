{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day1Spec (spec) where

import AdventOfCode.Day1
import Data.Functor ((<&>))
import Test.Hspec

spec :: Spec
spec = do
  describe "test day 1" $ do
    it "part one" $
      (readFile "test/Data/Day1.txt" <&> part1) `shouldReturn` 24000
    it "part two" $
      (readFile "test/Data/Day1.txt" <&> part2) `shouldReturn` 45000
