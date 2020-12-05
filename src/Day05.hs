-- | 

module Day05 (day05, run05a, run05b, decodeSeat) where

import Data.List (foldl')
import qualified Data.Set as Set

day05 input = unlines
  [ "Part 1: " ++ (show $ run05a input)
  , "Part 2: " ++ (show $ run05b input)
  ]

run05a :: String -> Int
run05a input = maximum $ seatId <$> decodeSeat <$> lines input

run05b :: String -> [(Int, Int, Int)]
run05b input = map withId $ inMiddle $ Set.toList $ allSeats `Set.difference` filledSeats
  where
    inMiddle = filter f -- TODO generalize, this was determined experimentally
      where f (row, _) = row > 5 && row < 100
    withId seat@(row, col) = (row, col, seatId seat)
    allSeats = Set.fromList [(a, b) | a <- [0..127], b <- [0..7]]
    filledSeats = Set.fromList $ decodeSeat <$> lines input
    

seatId :: (Int, Int) -> Int
seatId (row, column) = (row * 8) + column

decodeSeat :: String -> (Int, Int)
decodeSeat xs = fix $ foldl' f (allRows, allCols) xs
  where
    fix ((rowL, rowH), (colL, colH))
      | rowL == rowH && colL == colH = (rowL, colL)
      | otherwise = error "Range not narrowed, instructions incomplete"
      
    f (rowRange, colRange) 'F' = (lowerHalf rowRange, colRange)
    f (rowRange, colRange) 'B' = (upperHalf rowRange, colRange)
    f (rowRange, colRange) 'R' = (rowRange, upperHalf colRange)
    f (rowRange, colRange) 'L' = (rowRange, lowerHalf colRange)

    lowerHalf (a, b) = (a, b - (range `div` 2)) where range = (b - a) + 1
    upperHalf (a, b) = (a + (range `div` 2), b) where range = (b - a) + 1

allRows = (0, 127)
allCols = (0, 7)