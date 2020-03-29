module Day1 where

fuelForMass :: Int -> Int
fuelForMass mass | fuel < 0  = 0
                 | otherwise = fuel
  where fuel = floor (fromIntegral mass / 3) - 2

fuelForMassPlusFuelForFuel :: Int -> Int
fuelForMassPlusFuelForFuel mass = recurse (fuelForMass mass, 0)
 where
  recurse (0, runningTotal) = runningTotal
  recurse (fuel, runningTotal) =
    recurse (fuelForMass fuel, runningTotal + fuel)

fuelForMasses :: [Int] -> Int
fuelForMasses = foldr ((+) . fuelForMassPlusFuelForFuel) 0
