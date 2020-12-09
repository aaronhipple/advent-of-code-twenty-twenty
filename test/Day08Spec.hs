-- |

module Day08Spec (spec) where

import Test.Hspec
import Day08

input :: String
input = unlines
  [ "nop +0"
  , "acc +1"
  , "jmp +4"
  , "acc +3"
  , "jmp -3"
  , "acc -99"
  , "acc +1"
  , "jmp -4"
  , "acc +6"
  ]
  
spec = describe "Day 8" $ do
  describe "part 1" $ do
    it "should get the right answer from the example" $ do
      run08a input `shouldBe` "Just 5"
  describe "part 2" $ do
    it "should get the right answer from the example" $ do
      run08b input `shouldBe` "Just [8]"
