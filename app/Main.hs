module Main where

import           Day1
import           Day1Input
import           Day2

main :: IO ()
main = do
  putStrLn ("Day 1: " ++ show (fuelForMasses moduleMasses))
  putStrLn ("Day 2 part 1: " ++ show day2Part1)
  putStrLn ("Day 2 part 2: " ++ show day2Part2)

