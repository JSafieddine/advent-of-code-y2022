{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day8Spec (spec) where

import AdventOfCode.Day8
import Data.Functor ((<&>))
import Test.Hspec

spec :: Spec
spec = do
  describe "test day 8" $ do
    it "part one" $
      (readFile "test/Data/Day8.txt" <&> part1) `shouldReturn` 21
    it "part two" $
      (readFile "test/Data/Day8.txt" <&> part2) `shouldReturn` 8
