-- | 

module Day03 (day03, run03a, run03b) where

day03 input = unlines
  [ "Part 1: " ++ (show $ run03a input (3, 1))
  , "Part 2: " ++ (show $ run03b input [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
  ]

run03a :: String -> (Int, Int) -> Int
run03a input (right, down) = length $ filter treeIsAt positions
  where
    field = lines input
    positions = takeWhile inBounds $ iterate tick (0, 0)
    tick (x, y) = (x + right, y + down)
    inBounds (_, y) = y < yDimension
    yDimension = length field
    xDimension = length (field !! 0)

    treeIsAt (x, y) = '#' == (field !! y !! (x `mod` xDimension))
    
run03b :: String -> [(Int, Int)] -> Int
run03b input slopes = product $ map (run03a input) slopes
