module Day3 where

import           Day3Input

data Section = RRight Int
             | UUp Int
             | LLeft Int
             | DDown Int
             deriving Show

toSections :: [String] -> [Section]
toSections = map (\x -> f x) where
  f (c : cs) | c == 'R' = RRight (read cs :: Int)
             | c == 'U' = UUp (read cs :: Int)
             | c == 'L' = LLeft (read cs :: Int)
             | c == 'D' = DDown (read cs :: Int)

sectionToCoordinates :: Section -> (Int, Int) -> [(Int, Int)]
sectionToCoordinates section (x, y) = case section of
  (RRight n) -> map (\i -> (x + i, y)) [1 .. n]
  (UUp    n) -> map (\i -> (x, y + i)) [1 .. n]
  (LLeft  n) -> map (\i -> (x - i, y)) [1 .. n]
  (DDown  n) -> map (\i -> (x, y - i)) [1 .. n]

wireToCoordinates :: [String] -> [(Int, Int)]
wireToCoordinates wire = f (toSections wire) (0, 0) []
 where
  f [] _ acc = acc
  f (w : ws) (x, y) acc =
    let newSection = sectionToCoordinates w (x, y)
    in  f ws (last newSection) (acc ++ newSection)

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

day3Part1 :: Int
day3Part1 =
  let (wire1, wire2) =
          (wireToCoordinates puzzleInput1, wireToCoordinates puzzleInput2)
  in  minimum . (map manhattanDistance) $ (filter (\n -> n `elem` wire2) wire1)
