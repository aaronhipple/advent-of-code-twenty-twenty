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
import Day09
import Day10
import Day11
import Day12

runDay :: Int -> String -> Either String String
runDay 1 x = Right $ day01 x
runDay 2 x = Right $ day02 x
runDay 3 x = Right $ day03 x
runDay 4 x = Right $ day04 x
runDay 5 x = Right $ day05 x
runDay 6 x = Right $ day06 x
runDay 7 x = Right $ day07 x
runDay 8 x = Right $ day08 x
runDay 9 x = Right $ day09 x
runDay 10 x = Right $ day10 x
runDay 11 x = Right $ day11 x
runDay 12 x = Right $ day12 x
runDay _ _ = Left "Unknown Day"
