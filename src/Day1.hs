module Day1 where

import           Day1Input

fuelForMass :: Int -> Int
fuelForMass mass | fuel < 0  = 0
                 | otherwise = fuel
  where fuel = floor (fromIntegral mass / 3) - 2

day1Part1 :: Int
day1Part1 = sum $ map fuelForMass moduleMasses

fuelForMassPlusFuelForFuel :: Int -> Int
fuelForMassPlusFuelForFuel mass = f (fuelForMass mass, 0)
 where
  f (0   , acc) = acc
  f (fuel, acc) = f (fuelForMass fuel, acc + fuel)

fuelForMasses :: [Int] -> Int
fuelForMasses = foldr ((+) . fuelForMassPlusFuelForFuel) 0

day1Part2 :: Int
day1Part2 = fuelForMasses moduleMasses
