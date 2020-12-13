-- |

module Day12Spec (spec) where

import Test.Hspec
import Day12

input :: String
input = unlines
  [ "F10"
  , "N3"
  , "F7"
  , "R90"
  , "F11"
  ]
  
spec = describe "Day 12" $ do
  describe "part 1" $ do
    it "should get the right answer from the example" $ do
      run12a input `shouldBe` "Just 25"
  describe "part 2" $ do
    it "should get the right answer from the example" $ do
      run12b input `shouldBe` "Just 286"
