module Lib
    ( runDay
    ) where

import Day01

runDay :: Int -> String -> Either String String
runDay 1 x = Right $ day01 x
runDay _ _ = Left "Unknown Day"
