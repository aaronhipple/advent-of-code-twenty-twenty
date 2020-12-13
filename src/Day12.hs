-- | 

module Day12  where

import Data.List (foldl')
import Parsing

day12 :: String -> String
day12 input = unlines
  [ "Part 1: " ++ (show $ run12a input)
  , "Part 2: " ++ (show $ run12b input)
  ]
    
run12a :: String -> String
run12a = run coords

run12b :: String -> String
run12b = run shipCoords
  
run :: Tickable a => (a -> (Int, Int)) -> String -> String
run f input = show $ manhattan (0, 0) <$> f <$> endState
  where
    endState = foldl' tick initialState <$> instrs
    instrs = runParse instructions input

class Tickable f where
  tick :: f -> Instruction -> f
  initialState :: f

data State1 = S
  { coords :: (Int, Int)
  , dir :: Dir
  } deriving (Show)

data State2 = R
  { shipCoords :: (Int, Int)
  , waypointCoords :: (Int, Int)
  , dir_ :: Dir
  } deriving (Show)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

instance Tickable State1 where
  tick (S coords' facing') ins = (S nextCoords nextFacing)
    where
      nextFacing = turn facing' ins
      nextCoords = move ins facing' coords'

      move (Direction d n) _ cs = mv d n cs
      move (Forward n) fc cs = mv fc n cs
      move (TurnLeft _) _ cs = cs
      move (TurnRight _) _ cs = cs

      distance (Forward n) = n
      distance (Direction _ n) = n
      distance (TurnLeft _) = 0
      distance (TurnRight _) = 0

      turn :: Dir -> Instruction -> Dir
      turn fc (Direction _ _) = fc
      turn fc (Forward _) = fc
      turn fc (TurnLeft n) = iterate turnL fc !! ticks
        where ticks = (n `mod` 360) `div` 90
      turn fc (TurnRight n) = iterate turnR fc !! ticks
        where ticks = (n `mod` 360) `div` 90
  
  initialState = S (0, 0) East

instance Tickable State2 where
  tick st (Direction d n) = st { waypointCoords = mv d n (waypointCoords st) }
  tick st (TurnLeft n) = st { waypointCoords = iterate rotL (waypointCoords st) !! ticks }
    where ticks = (n `mod` 360) `div` 90
  tick st (TurnRight n) = st { waypointCoords = iterate rotR (waypointCoords st) !! ticks }
    where ticks = (n `mod` 360) `div` 90
  tick st (Forward n) = st { shipCoords = iterate (go (waypointCoords st)) (shipCoords st) !! n }
    where
      go (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  
  initialState = R (0, 0) (10, 1) East

mv :: Dir -> Int -> (Int, Int) -> (Int, Int)   
mv North n (x, y) = (x, y + n)
mv South n (x, y) = (x, y - n)
mv East n (x, y) = (x + n, y)
mv West n (x, y) = (x - n, y)

rotL :: (Int, Int) -> (Int, Int)
rotL (x, y) = (-y, x)
  
rotR :: (Int, Int) -> (Int, Int)
rotR (x, y) = (y, -x)
  
turnL :: Dir -> Dir
turnL facing
  | facing == minBound = maxBound
  | otherwise = pred facing
  
turnR :: Dir -> Dir
turnR facing
  | facing == maxBound = minBound
  | otherwise = succ facing

data Dir = North | East | South | West
  deriving (Enum, Bounded, Eq, Show)

data Instruction = Forward Int | TurnLeft Int | TurnRight Int | Direction Dir Int
  deriving (Show)

instructions :: Parser [Instruction]
instructions = many instruction

instruction :: Parser Instruction
instruction = token $ (p <*> nat)
  where
    p =  (char 'N' *> pure (Direction North))
     <|> (char 'S' *> pure (Direction South))
     <|> (char 'E' *> pure (Direction East))
     <|> (char 'W' *> pure (Direction West))
     <|> (char 'L' *> pure TurnLeft)
     <|> (char 'R' *> pure TurnRight)
     <|> (char 'F' *> pure Forward)


