-- |

module Day10Spec (spec) where

import Test.Hspec
import Day10


example1a :: String
example1a = unlines
  [ "16"
  , "10"
  , "15"
  , "5"
  , "1"
  , "11"
  , "7"
  , "19"
  , "6"
  , "12"
  , "4"
  ]

example1b :: String
example1b = unlines
  [ "28"
  , "33"
  , "18"
  , "42"
  , "31"
  , "14"
  , "46"
  , "20"
  , "48"
  , "47"
  , "24"
  , "23"
  , "49"
  , "45"
  , "19"
  , "38"
  , "39"
  , "11"
  , "1"
  , "32"
  , "25"
  , "35"
  , "8"
  , "17"
  , "7"
  , "9"
  , "4"
  , "2"
  , "34"
  , "10"
  , "3"
  ]
  
spec = describe "Day 10" $ do
  describe "part 1" $ do
    it "should get the right answer from the first example" $ do
      run10a example1a `shouldBe` "((7,0,5),35)"
    it "should get the right answer from the second example" $ do
      run10a example1b `shouldBe` "((22,0,10),220)"
  describe "part 2" $ do
    it "should get the right answer from a toy example" $ do
      run10b (unlines ["2", "3", "5"]) `shouldBe` "3"
    it "should get the right answer from the first example" $ do
      run10b example1a `shouldBe` "8"
    it "should get the right answer from the second example" $ do
      run10b example1b `shouldBe` "19208"
