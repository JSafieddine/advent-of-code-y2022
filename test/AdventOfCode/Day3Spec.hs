{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day3Spec (spec) where

import AdventOfCode.Day3
import Data.Functor ((<&>))
import Test.Hspec

spec :: Spec
spec = do
  describe "test day 3" $ do
    it "part one" $
      (readFile "test/Data/Day3.txt" <&> part1) `shouldReturn` 157
    it "part two" $
      (readFile "test/Data/Day3.txt" <&> part2) `shouldReturn` 70
