-- |

module Day06Spec (spec) where

import Test.Hspec
import Day06

input :: String
input = unlines
  ["abc"
  , ""
  , "a"
  , "b"
  , "c"
  , ""
  , "ab"
  , "ac"
  , ""
  , "a"
  , "a"
  , "a"
  , "a"
  , ""
  , "b"
  ]


spec = describe "Day 6" $ do
  describe "part 1" $ do
    it "should get the right answer from the example" $ do
      run06a input `shouldBe` 11
  describe "part 2" $ do
    it "should get the right answer from the example" $ do
      run06b input `shouldBe` 6
