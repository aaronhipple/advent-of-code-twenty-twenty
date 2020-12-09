-- |

module Day09Spec (spec) where

import Test.Hspec
import Day09

input :: String
input = unlines
  [ "35"
  , "20"
  , "15"
  , "25"
  , "47"
  , "40"
  , "62"
  , "55"
  , "65"
  , "95"
  , "102"
  , "117"
  , "150"
  , "182"
  , "127"
  , "219"
  , "299"
  , "277"
  , "309"
  , "576"
  ]
  
spec = describe "Day 9" $ do
  describe "part 1" $ do
    it "should get the right answer from the example" $ do
      run09a 5 input `shouldBe` "127"
  describe "part 2" $ do
    it "should get the right answer from the example" $ do
      run09b 5 input `shouldBe` "([15,25,47,40],62)"
