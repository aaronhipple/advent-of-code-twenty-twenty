-- | 

module Day02 (day02, run02a, run02b) where

import Parsing

day02 input = unlines
  [ "Part 1: " ++ (show $ run02a input)
  , "Part 2: " ++ (show $ run02b input)
  ]
  

run :: (Policy -> String -> Bool) -> String -> Maybe Int
run check x = case runParse lines x of
  Nothing -> Nothing
  Just parseResult -> Just $ length $ filter applyCheck parseResult
  where
    lines :: Parser [(Policy, String)]
    lines = many line
    applyCheck (policy, password) = check policy password
    
run02a = run checkPolicyA
run02b = run checkPolicyB

checkPolicyA :: Policy -> String -> Bool
checkPolicyA (Policy (minC, maxC, c)) password = minC <= countC && countC <= maxC
  where
    countC = length $ filter (==c) password

checkPolicyB :: Policy -> String -> Bool
checkPolicyB (Policy (posA, posB, c)) password = (charA == c && charB /= c) || (charA /= c && charB == c)
  where
    charA = password !! (posA - 1)
    charB = password !! (posB - 1)

newtype Policy = Policy (Int, Int, Char)

pol :: Parser Policy
pol = do
  minC <- nat
  char '-'
  maxC <- nat
  space
  c <- letter
  return $ Policy (minC, maxC, c)

line :: Parser (Policy, String)
line = do
  policy <- pol
  char ':'
  space
  password <- token identifier
  return $ (policy, password)
