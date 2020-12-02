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

spec = describe "run02a" $ do
  it "produces the expected output" $ do
    run01a part1input `shouldBe` Just 2
