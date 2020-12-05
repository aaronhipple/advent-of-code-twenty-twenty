-- | 

module Day05 (day05, run05a, run05b, decodeSeat) where

import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

day05 :: String -> String
day05 input = unlines
  [ "Part 1: " ++ (show $ run05a input)
  , "Part 2: " ++ (show $ run05b input)
  ]

run05a :: String -> Int
run05a input = maximum $ seatId <$> decodeSeat <$> lines input

run05b :: String -> [(Int, Int, Int)]
run05b input = map withId $ nonEmptyRows $ Set.toList $ allSeats `Set.difference` filledSeats
  where
    withId seat@(row, col) = (row, col, seatId seat)
    allSeats = Set.fromList [(a, b) | a <- [0..127], b <- [0..7]]
    filledSeats = Set.fromList $ decodeSeat <$> lines input

nonEmptyRows :: [(Int, Int)] -> [(Int, Int)]
nonEmptyRows seats = 
  [ (row, col)
  | (row, cols) <- filter (\(_, cols) -> Set.size cols /= 8) $ Map.toList rowMap
  , col <- Set.toList cols
  ]
  where
    rowMap = foldl' f Map.empty seats
    f m (row, col) = Map.insertWith Set.union row (Set.singleton col) m

seatId :: (Int, Int) -> Int
seatId (row, column) = (row * 8) + column

decodeSeat :: String -> (Int, Int)
decodeSeat xs = fromRanges $ foldl' partition (allRows, allCols) xs

fromRanges :: ((Int, Int), (Int, Int)) -> (Int, Int)
fromRanges ((rowL, rowH), (colL, colH))
  | rowL == rowH && colL == colH = (rowL, colL)
  | otherwise = error "Range not narrowed, instructions incomplete"
      
partition :: ((Int, Int), (Int, Int)) -> Char -> ((Int, Int), (Int, Int))
partition (rowRange, colRange) 'F' = (lowerHalf rowRange, colRange)
partition (rowRange, colRange) 'B' = (upperHalf rowRange, colRange)
partition (rowRange, colRange) 'R' = (rowRange, upperHalf colRange)
partition (rowRange, colRange) 'L' = (rowRange, lowerHalf colRange)
partition _ _ = error "unexpected input"

lowerHalf :: (Int, Int) -> (Int, Int)
lowerHalf (a, b) = (a, b - (range `div` 2)) where range = (b - a) + 1
  
upperHalf :: (Int, Int) -> (Int, Int)
upperHalf (a, b) = (a + (range `div` 2), b) where range = (b - a) + 1

allRows :: (Int, Int)
allRows = (0, 127)
  
allCols :: (Int, Int)
allCols = (0, 7)
