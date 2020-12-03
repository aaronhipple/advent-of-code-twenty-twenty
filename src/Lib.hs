module Lib
    ( runDay
    ) where

import Day01
import Day02

runDay :: Int -> String -> Either String String
runDay 1 x = Right $ day01 x
runDay 2 x = Right $ day02 x
runDay _ _ = Left "Unknown Day"