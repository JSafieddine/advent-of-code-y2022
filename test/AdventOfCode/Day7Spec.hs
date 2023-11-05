{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day7Spec (spec) where

import AdventOfCode.Day7
import Data.Functor ((<&>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "test day 7" $ do
    it "cdParser 1" $
      parse cdParser "" "$ cd /\n" `shouldParse` CD "/"
    it "cdParser 2" $
      parse cdParser "" "$ cd ..\n" `shouldParse` CD ".."
    it "cdParser 3" $
      parse cdParser "" "$ cd abc\n" `shouldParse` CD "abc"
    it "contentParser 1" $
      parse contentParser "" "dir abc\n" `shouldParse` Dir "abc"
    it "contentParser 2" $
      parse contentParser "" "123 abc.def\n" `shouldParse` File 123 "abc.def"
    it "lsParser" $
      parse lsParser "" "$ ls\ndir abc\n123 abc.def\n" `shouldParse` LS [Dir "abc", File 123 "abc.def"]
    it "inputParser" $ do
      input <- readFile "test/Data/Day7.txt"
      parse inputParser "" input
        `shouldParse` [ CD "/",
                        LS
                          [ Dir "a",
                            File 14848514 "b.txt",
                            File 8504156 "c.dat",
                            Dir "d"
                          ],
                        CD "a",
                        LS
                          [ Dir "e",
                            File 29116 "f",
                            File 2557 "g",
                            File 62596 "h.lst"
                          ],
                        CD "e",
                        LS
                          [ File 584 "i"
                          ],
                        CD "..",
                        CD "..",
                        CD "d",
                        LS
                          [ File 4060174 "j",
                            File 8033020 "d.log",
                            File 5626152 "d.ext",
                            File 7214296 "k"
                          ]
                      ]
    it "part one" $
      (readFile "test/Data/Day7.txt" <&> part1) `shouldReturn` 95437
    it "part two" $
      (readFile "test/Data/Day7.txt" <&> part2) `shouldReturn` 24933642
