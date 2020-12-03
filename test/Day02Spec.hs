-- |

module Day02Spec (spec) where

import Test.Hspec
import Day02

part1input :: String
part1input = unlines
  [ "1-3 a: abcde"
  , "1-3 b: cdefg"
  , "2-9 c: ccccccccc"
  ]

spec = describe "Day 2" $ do
  describe "run02a" $ do
    it "produces the expected output" $ do
      run02a part1input `shouldBe` Just 2
  describe "run02b" $ do
    it "produces the expected output" $ do
      run02b part1input `shouldBe` Just 1
