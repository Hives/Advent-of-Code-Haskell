module Day1 where

import           Day1Input

fuelForMass :: Int -> Int
fuelForMass mass | fuel < 0  = 0
                 | otherwise = fuel
  where fuel = floor (fromIntegral mass / 3) - 2

day1Part1 :: Int
day1Part1 = sum $ map (\m -> fuelForMass m) moduleMasses

fuelForMassPlusFuelForFuel :: Int -> Int
fuelForMassPlusFuelForFuel mass = recurse (fuelForMass mass, 0)
 where
  recurse (0, runningTotal) = runningTotal
  recurse (fuel, runningTotal) =
    recurse (fuelForMass fuel, runningTotal + fuel)

fuelForMasses :: [Int] -> Int
fuelForMasses = foldr ((+) . fuelForMassPlusFuelForFuel) 0

day1Part2 :: Int
day1Part2 = fuelForMasses moduleMasses
