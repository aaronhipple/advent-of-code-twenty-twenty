-- | 

module Day06 (day06, run06a, run06b) where

import Parsing
import Data.Set (Set)
import qualified Data.Set as Set

day06 :: String -> String
day06 input = unlines
  [ "Part 1: " ++ (show $ run06a input)
  , "Part 2: " ++ (show $ run06b input)
  ]

run :: Parser [Set Char] -> String -> Int
run parser input = sum $ Set.size <$> answers
  where
    answers = maybe [] id $ runParse parser input
    
run06a :: String -> Int
run06a = run allAnswersDisjunctive
  
run06b :: String -> Int
run06b = run allAnswersConjunctive
    
personAnswers :: Parser (Set Char)
personAnswers = do
  answers <- some alphanum
  skip (char '\n') <|> eof
  return $ Set.fromList answers
  
allAnswers :: ([Set Char] -> Set Char) -> Parser [Set Char]
allAnswers collect = some groupAnswers
 where 
  groupAnswers = do
    persons <- some personAnswers
    skip (char '\n') <|> eof
    return $ collect persons

allAnswersDisjunctive :: Parser [Set Char]
allAnswersDisjunctive = allAnswers $ foldr Set.union Set.empty

allAnswersConjunctive :: Parser [Set Char]
allAnswersConjunctive = allAnswers f
 where 
  f [] = error "called on empty group"
  f (p:ps) = foldr Set.intersection p ps

