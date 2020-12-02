module Day01 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

day01 input = unlines
  [ printResult $ run01a 2020 inputNumbers
  , printResult $ run01b 2020 inputNumbers
  ] 
  where
    printResult Nothing = "No result"
    printResult (Just result) = unlines [show result, show (List.product result)]
    
    inputNumbers = fmap read $ lines input

run01a :: Int -> [Int] -> Maybe [Int]
run01a target ns = makeResult <$> match
  where
    makeResult a = [a, target - a]
    match = List.find f ns
    f n
      | (target - n) `Set.member` bs = True
      | otherwise = False
    bs = Set.fromList ns

run01b :: Int -> [Int] -> Maybe [Int]
run01b target ns = makeResult <$> match
  where
    makeResult ms = target - (List.sum ms) : ms
    match = head $ dropWhile Maybe.isNothing $ List.map f ns
    f n = run01a (target - n) ns
