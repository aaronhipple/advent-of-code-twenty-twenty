-- |

module Day11Spec (spec) where

import Test.Hspec
import Day11

input :: String
input = unlines
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]
  
spec = describe "Day 11" $ do
  describe "part 1" $ do
    it "should get the right answer from the example" $ do
      run11a input `shouldBe` "37"
  describe "part 2" $ do
    it "should get the right answer from the example" $ do
      run11b input `shouldBe` "26"
