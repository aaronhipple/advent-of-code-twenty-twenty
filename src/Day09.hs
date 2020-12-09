-- | 

module Day09 (day09, run09a, run09b, findRange) where

import Data.List

import qualified Data.Set as Set

day09 :: String -> String
day09 input = unlines
  [ "Part 1: " ++ (show $ run09a 25 input)
  , "Part 2: " ++ (show $ run09b 25 input)
  ]
    
run09a :: Int -> String -> String
run09a n input = show $ findInvalid n ints
  where ints = getInts input
  
run09b :: Int -> String -> String
run09b n input = show $ (range, weakness)
  where ints = getInts input
        target = findInvalid n ints
        range = findRange target ints
        weakness = minimum range + maximum range

findInvalid :: Int -> [Int] -> Int
findInvalid len ns
  | length priors < len = error "hit the end of the list"
  | x `Set.member` sums = findInvalid len (ps ++ (x:xs))
  | otherwise = x
  where
    sums = Set.fromList [a + b | a <- priors, b <- filter (/= a) priors]
    (priors@(p:ps), (x:xs)) = splitAt len ns

findRange :: Int -> [Int] -> [Int]
findRange target ns = take ((j - i) + 1) $ drop i $ ns
  where
    (i, j, _) = head $ filter (\(i', j', t) -> t == target && i' /= j') (concat it)
  
    it :: [[(Int, Int, Int)]]
    it = scan <$> tails indexed

    indexed :: [(Int, Int, Int)]
    indexed = zip3 [0..] [0..] ns
    
    scan :: [(Int, Int, Int)] -> [(Int, Int, Int)]
    scan = scanl1 f
    f :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    f (i1, _, a) (_, j2, b) = (i1, j2, a + b)
    
  

getInts :: String -> [Int]
getInts = map read . lines
