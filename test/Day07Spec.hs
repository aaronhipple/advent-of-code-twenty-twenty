-- |

module Day07Spec (spec) where

import Test.Hspec
import Day07

input :: String
input = unlines
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]
  
input2 :: String
input2 = unlines
  [ "shiny gold bags contain 2 dark red bags."
  , "dark red bags contain 2 dark orange bags."
  , "dark orange bags contain 2 dark yellow bags."
  , "dark yellow bags contain 2 dark green bags."
  , "dark green bags contain 2 dark blue bags."
  , "dark blue bags contain 2 dark violet bags."
  , "dark violet bags contain no other bags."
  ]

spec = describe "Day 7" $ do
  describe "part 1" $ do
    it "should get the right answer from the example" $ do
      run07a input `shouldBe` "Just 4"
  describe "part 2" $ do
    it "should get the right answer from the example" $ do
      run07b input `shouldBe` "Just 32"
    it "should get the right answer from the second example" $ do
      run07b input2 `shouldBe` "Just 126"
