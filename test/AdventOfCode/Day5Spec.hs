{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day5Spec (spec) where

import AdventOfCode.Day5
import Data.Functor ((<&>))
import Data.Map (fromAscList)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "test day 5" $ do
    it "crateParser case 1" $
      parse crateParser "" "[A]" `shouldParse` Just 'A'
    it "crateParser case 2" $
      parse crateParser "" "   " `shouldParse` Nothing
    it "crateParser case 3" $
      parse crateParser "" `shouldFailOn` " 1 "
    it "crateLineParser case 1" $
      parse crateLineParser "" "[A]     [C]\n" `shouldParse` [Just 'A', Nothing, Just 'C']
    it "stackNumberParser" $
      parse stackNumberParser "" " 1 " `shouldParse` 1
    it "stackNumberLineParser" $
      parse stackNumberLineParser "" " 1   2 \n" `shouldParse` [1, 2]
    it "crateStackParser" $ do
      input <- readFile "test/Data/Day5.txt"
      parse crateStackParser "" input
        `shouldParse` fromAscList
          [ (1, "NZ"),
            (2, "DCM"),
            (3, "P")
          ]
    it "movementParser" $
      parse movementParser "" "move 1 from 2 to 1\n" `shouldParse` Move 1 2 1
    it "inputParser test" $ do
      input <- readFile "test/Data/Day5.txt"
      parse inputParser "" input
        `shouldParse` ( fromAscList
                          [ (1, "NZ"),
                            (2, "DCM"),
                            (3, "P")
                          ],
                        [ Move 1 2 1,
                          Move 3 1 3,
                          Move 2 2 1,
                          Move 1 1 2
                        ]
                      )
    it "part one" $
      (readFile "test/Data/Day5.txt" <&> part1) `shouldReturn` "CMZ"
    it "part two" $
      (readFile "test/Data/Day5.txt" <&> part2) `shouldReturn` "MCD"
