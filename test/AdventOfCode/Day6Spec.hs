{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day6Spec (spec) where

import AdventOfCode.Day6
import Data.Functor ((<&>))
import Test.Hspec

spec :: Spec
spec = do
  describe "test day 6" $ do
    it "part one case 1" $
      part1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 7
    it "part one case 2" $
      part1 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
    it "part one case 3" $
      part1 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
    it "part one case 4" $
      part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
    it "part one case 4" $
      part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11
    it "part two case 1" $
      part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19
    it "part two case 2" $
      part2 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
    it "part two case 3" $
      part2 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 23
    it "part two case 4" $
      part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29
    it "part two case 4" $
      part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 26
