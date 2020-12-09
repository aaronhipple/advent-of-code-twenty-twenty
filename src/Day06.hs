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
    
run06a :: String -> Int
run06a = run allAnswersDisjunctive
  
run06b :: String -> Int
run06b = run allAnswersConjunctive

run :: Parser [Set Char] -> String -> Int
run parser input = sum $ Set.size <$> (maybe [] id $ runParse parser input)
  
allAnswersDisjunctive :: Parser [Set Char]
allAnswersDisjunctive = allAnswers $ foldr Set.union Set.empty

allAnswersConjunctive :: Parser [Set Char]
allAnswersConjunctive = allAnswers $ \(p:ps) -> foldr Set.intersection p ps

allAnswers :: ([Set Char] -> Set Char) -> Parser [Set Char]
allAnswers collect = some groupAnswers
 where 
  groupAnswers = collect <$> some personAnswers <* separator
  personAnswers = Set.fromList <$> some alphanum <* separator
  separator = skip (char '\n') <|> eof


