-- |

module Day05Spec (spec) where

import Test.Hspec
import Day05

spec = describe "Day 5" $ do
  describe "decodeSeat" $ do
    it "decodes BFFFBBFRRR correctly" $ do
      decodeSeat "BFFFBBFRRR" `shouldBe` (70, 7)
    it "decodes FFFBBBFRRR correctly" $ do
      decodeSeat "FFFBBBFRRR" `shouldBe` (14, 7)
    it "decodes BBFFBBFRLL correctly" $ do
      decodeSeat "BBFFBBFRLL" `shouldBe` (102, 4)
