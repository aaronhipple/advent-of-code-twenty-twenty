-- | 

module Day02 (day02, run01a) where

import Parsing

day02 input = unlines
  [ "Part 1: " ++ (show $ run01a input)
  ]

run01a :: String -> Maybe Int
run01a x = case runParse lines x of
  Nothing -> Nothing
  Just parseResult -> Just $ length $ filter applyCheck parseResult
  where
    lines :: Parser [(Policy, String)]
    lines = many line
    applyCheck (policy, password) = check policy password

runParse :: Parser a -> String -> Maybe a
runParse p xs = case parse p xs of
  [(a, [])] -> Just a
  _ -> Nothing

check :: Policy -> String -> Bool
check (Policy (minC, maxC, c)) password = minC <= countC && countC <= maxC
  where
    countC = length $ filter (==c) password

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
