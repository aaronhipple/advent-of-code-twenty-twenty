module Lib
    ( runDay
    ) where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08

runDay :: Int -> String -> Either String String
runDay 1 x = Right $ day01 x
runDay 2 x = Right $ day02 x
runDay 3 x = Right $ day03 x
runDay 4 x = Right $ day04 x
runDay 5 x = Right $ day05 x
runDay 6 x = Right $ day06 x
runDay 7 x = Right $ day07 x
runDay 8 x = Right $ day08 x
runDay _ _ = Left "Unknown Day"
