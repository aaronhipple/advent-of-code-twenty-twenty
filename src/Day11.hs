-- | 

module Day11 (day11, run11a, run11b) where

import Control.Applicative

day11 :: String -> String
day11 input = unlines
  [ "Part 1: " ++ (show $ run11a input)
  , "Part 2: " ++ (show $ run11b input)
  ]
    
run11a :: String -> String
run11a = run tick1

run11b :: String -> String
run11b = run tick2

run :: (Seats -> Seats) -> String -> String
run tickFn input = show $ countOccupied $ fst $ head $ dropWhile (uncurry (/=)) $ zip ticks (tail ticks)
  where
    grid = readInput input
    ticks = iterate tickFn grid
    countOccupied (Seats xss) = length $ filter (== Occupied) $ concat $ xss

tick1 :: Seats -> Seats
tick1 (Seats xss) = Seats $ (fmap . fmap) (uncurry f) xss'
  where
    xss' = zipWith zip xss occupiedNeighbors

    f Empty n = if n == 0 then Occupied else Empty
    f Occupied n = if n >= 4 then Empty else Occupied
    f x _ = x
    
    occupiedNeighbors :: [[Int]]
    occupiedNeighbors = (fmap . fmap) (length . filter (== Occupied)) nss
      where
        nss :: [[[Seat]]]
        nss = fromZip $ fmap sequenceA $ sequenceA $ toZip
          [ rollN xss
          , (rollN . rollE) xss
          , rollE xss
          , (rollS . rollE) xss
          , rollS xss
          , (rollS . rollW) xss
          , rollW xss
          , (rollN . rollW) xss
          ]
    
tick2 :: Seats -> Seats
tick2 (Seats xss) = Seats $ (fmap . fmap) (uncurry f) xss'
  where
    xss' = zipWith zip xss occupiedNeighbors

    f Empty n = if n == 0 then Occupied else Empty
    f Occupied n = if n >= 5 then Empty else Occupied
    f x _ = x
    
    occupiedNeighbors :: [[Int]]
    occupiedNeighbors = (fmap . fmap) (length . filter (== Occupied)) nss
      where
        nss :: [[[Seat]]]
        nss = fromZip $ fmap sequenceA $ sequenceA $ toZip
          [ cast rollN xss
          , cast (rollN . rollE) xss
          , cast rollE xss
          , cast (rollS . rollE) xss
          , cast rollS xss
          , cast (rollS . rollW) xss
          , cast rollW xss
          , cast (rollN . rollW) xss
          ]

cast :: ([[Seat]] -> [[Seat]]) -> [[Seat]] -> [[Seat]]
cast fn xss = fn $ fst $ head $ dropWhile (uncurry (/=)) $ zip casts (tail casts)
  where
    casts = iterate f xss
    f xss' = coalesce' xss' (fn xss')
    
    coalesce' yss zss = (fmap . fmap) (uncurry coalesce) $ zipWith zip yss zss
    coalesce Floor y = y
    coalesce x _ = x

rollN = rollIn (map (const Floor) . head)
rollS = rollOut (map (const Floor) . head)
rollE xs = rollOut (const Floor) <$> xs
rollW xs = rollIn (const Floor) <$> xs

rollIn fillFn xs = tail xs ++ [fillFn xs]
rollOut fillFn xs = fillFn xs : take (length xs - 1) xs

toZip xsss = ZipList $ ZipList <$> (fmap . fmap) ZipList xsss
fromZip xsss = getZipList $ getZipList <$> (fmap . fmap) getZipList xsss
        
readInput :: String -> Seats
readInput x = Seats $ (fmap . fmap) fromChar (lines x)

data Seat = Floor | Empty | Occupied
  deriving Eq

fromChar :: Char -> Seat
fromChar '.' = Floor
fromChar 'L' = Empty
fromChar '#' = Occupied
fromChar _ = error "Unknown character"

instance Show (Seat)
  where
    show Floor = "."
    show Empty = "L"
    show Occupied = "#"

newtype Seats = Seats [[Seat]]
  deriving Eq

instance Show (Seats)
  where
    show (Seats x) = unlines $ (>>= show) <$> x
