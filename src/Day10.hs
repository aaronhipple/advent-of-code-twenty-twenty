-- | 

module Day10 (day10, run10a, run10b) where

import Data.List (sort, tails)

day10 :: String -> String
day10 input = unlines
  [ "Part 1: " ++ (show $ run10a input)
  , "Part 2: " ++ (show $ run10b input)
  ]
    
run10a :: String -> String
run10a input = show $ (diffs, c1 * c3)
  where
    diffs@(c1, _, c3) = findDiffs ns
    ns :: [Int]
    ns = read <$> lines input

findDiffs :: [Int] -> (Int, Int, Int)
findDiffs ns = foldl f (0, 0, 0) diffs
  where
    f _ 0 = error "duplicate item"
    f (c1, c2, c3) 1 = (c1 + 1, c2, c3)
    f (c1, c2, c3) 2 = (c1, c2 + 1, c3)
    f (c1, c2, c3) 3 = (c1, c2, c3 + 1)
    f _ n = error $ "unknown jump " ++ show n
    
    diffs = zipWith (-) (tail allNs) allNs
    allNs = sort $ 0 : mine : ns
    mine = 3 + maximum ns
  
run10b :: String -> String
run10b input = show $ findWays allNs
  where
    allNs = sort $ 0 : mine : ns
    mine = 3 + maximum ns
    
    ns :: [Int]
    ns = read <$> lines input

-- assumes input is sorted
findWays :: [Int] -> Int
findWays ns = ways !! 0
  where
    ways = map countWays $ zip [0..] rawWays
  
    countWays (i, nways)
      | i == max_i = 1
      | otherwise = sum $ (ways !!) <$> nextIndices
        where
          nextIndices = map (+ i) [1 .. nways]

    max_i = length rawWays - 1
      
    rawWays = map f $ filter (not . null) (tails ns)
    f [] = 0
    f [_] = 1
    f (x:xs) = length $ takeWhile (<= (x + 3)) xs
