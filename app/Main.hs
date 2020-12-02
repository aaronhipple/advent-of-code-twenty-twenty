module Main where

import System.Environment
import System.Exit

import Data.Char (isDigit)

import Lib

main :: IO ()
main = getArgs >>= handleArgs >>= doDay

doDay (n, input) = case runDay n input of
                     Left error -> putStrLn error >> exitWith (ExitFailure 1)
                     Right result -> putStrLn result

handleArgs ["-h"] = usage >> exitWith ExitSuccess
handleArgs [x, path]
  | all isDigit x = do
      contents <- readFile path
      pure (read x, contents)
  | otherwise = exitWith (ExitFailure 1)
handleArgs _ = usage >> exitWith (ExitFailure 1)

usage = putStrLn "Usage: advent [-h] [day] [input]"
