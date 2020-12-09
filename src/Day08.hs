-- | 

module Day08 (day08, run08a, run08b) where

import Control.Monad.State
import Parsing

import Data.Maybe (mapMaybe)

import Data.Set (Set)
import qualified Data.Set as Set
  
import Data.Map (Map)
import qualified Data.Map as Map

day08 :: String -> String
day08 input = unlines
  [ "Part 1: " ++ (show $ run08a input)
  , "Part 2: " ++ (show $ run08b input)
  ]
    
run08a :: String -> String
run08a input = show $ do
  pr <- runParse program input
  let pl = Map.fromList $ zip [0..] pr
  let startState = PS pl 0 0 Set.empty
  pure $ evalState (runUntilLoop f) startState
  where
    f True st = accum st
    f False _ = error "unknown instruction"
  
run08b :: String -> String
run08b input = show $ do
  pr <- runParse program input
  let pl = Map.fromList $ zip [0..] pr
  let programs = [Map.alter (fmap flip') n pl | n <- Map.keys pl, canFlip (n `Map.lookup` pl)]
  pure $ mapMaybe stoppedValue programs
  where
    stoppedValue :: Program -> Maybe Int
    stoppedValue pr = evalState (runUntilLoop f) (PS pr 0 0 Set.empty)
  
    f False st = Just (accum st)
    f True _ = Nothing
    

flip' :: Op -> Op
flip' (Nop n) = Jmp n
flip' (Jmp n) = Nop n
flip' op = op
  
canFlip :: Maybe Op -> Bool
canFlip (Just (Nop n)) = True
canFlip (Just (Jmp n)) = True
canFlip op = False
    
runUntilLoop :: (Bool -> ProgramState -> a) -> State ProgramState a
runUntilLoop fn = do
  st <- get
  let (PS prg acum ct vis) = st
  let newVis = ct `Set.insert` vis
  if ct `Set.member` vis then
    pure $ fn True st
  else 
    case Map.lookup ct prg of
      Just (Nop _) -> do
        put $ PS prg acum (ct + 1) newVis
        runUntilLoop fn
      Just (Acc n) -> do
        put $ PS prg (acum + n) (ct + 1) newVis
        runUntilLoop fn
      Just (Jmp n) -> do
        put $ PS prg acum (ct + n) newVis
        runUntilLoop fn
      _ -> pure $ fn False st

data ProgramState = PS
  { prog :: Map Int Op
  , accum :: Int
  , ctr :: Int
  , visited :: Set Int
  }
  deriving Show
  
type Program = Map Int Op
data Op = Nop Int | Acc Int | Jmp Int
  deriving Show

program :: Parser [Op]
program = many (token op)

op :: Parser Op
op = (Nop <$> (token (string "nop") *> cnt))
 <|> (Acc <$> (token (string "acc") *> cnt))
 <|> (Jmp <$> (token (string "jmp") *> cnt))

cnt :: Parser Int
cnt = positive <|> negative
  where positive = char '+' *> int
        negative = char '-' *> (((-1) *) <$> int)
