{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day2Spec (spec) where

import AdventOfCode.Day2
import Data.Functor ((<&>))
import Test.Hspec

spec :: Spec
spec = do
  describe "test day 2" $ do
    it "part one" $
      (readFile "test/Data/Day2.txt" <&> part1) `shouldReturn` 15
    it "part two" $
      (readFile "test/Data/Day2.txt" <&> part2) `shouldReturn` 12
