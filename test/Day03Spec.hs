-- |

module Day03Spec (spec) where

import Test.Hspec
import Day03

exampleInput :: String
exampleInput = unlines
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]

spec = describe "Day 3" $ do
  describe "run03a" $ do
    it "produces the expected output for the example" $ do
      run03a exampleInput (3, 1) `shouldBe` 7
  describe "run03b" $ do
    it "produces the expected output for the example" $ do
      run03b exampleInput [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] `shouldBe` 336
