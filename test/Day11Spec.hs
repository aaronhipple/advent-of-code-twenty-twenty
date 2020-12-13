-- |

module Day11Spec (spec) where

import Test.Hspec
import Day11
  
spec = describe "Day 11" $ do
  describe "part 1" $ do
    it "should get the right answer from the example" $ do
      run11a "" `shouldBe` ""
  describe "part 2" $ do
    it "should get the right answer from the example" $ do
      run11b "" `shouldBe` ""
