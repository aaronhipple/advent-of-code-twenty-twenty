module Lib
    ( runDay
    ) where

import Day01
import Day02
import Day03
import Day04

runDay :: Int -> String -> Either String String
runDay 1 x = Right $ day01 x
runDay 2 x = Right $ day02 x
runDay 3 x = Right $ day03 x
runDay 4 x = Right $ day04 x
runDay _ _ = Left "Unknown Day"
