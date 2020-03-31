module Day3 where

import           Day3Input

data Point = Point Int Int
             deriving (Show, Eq)

data Section = RRight Int
             | UUp Int
             | LLeft Int
             | DDown Int
             deriving Show

toSections :: [String] -> [Section]
toSections = map f where
  f (c : cs) | c == 'R' = RRight (read cs :: Int)
             | c == 'U' = UUp (read cs :: Int)
             | c == 'L' = LLeft (read cs :: Int)
             | c == 'D' = DDown (read cs :: Int)

sectionToPoints :: Section -> Point -> [Point]
sectionToPoints section (Point x y) = case section of
  (RRight n) -> map (\i -> Point (x + i) y) [n, (n - 1) .. 1]
  (UUp    n) -> map (\i -> Point x (y + i)) [n, (n - 1) .. 1]
  (LLeft  n) -> map (\i -> Point (x - i) y) [n, (n - 1) .. 1]
  (DDown  n) -> map (\i -> Point x (y - i)) [n, (n - 1) .. 1]

wireToPoints :: [String] -> [Point]
wireToPoints wire = f (toSections wire) (Point 0 0) []
 where
  f [] _ acc = acc
  f (w : ws) (Point x y) acc =
    let newSection = sectionToPoints w (Point x y)
    in  f ws (head newSection) (newSection ++ acc)

manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = abs x + abs y

example1 = ["R8", "U5", "L5", "D3"]
example2 = ["U7", "R6", "D4", "L4"]

day3Part1 :: Int
day3Part1 =
  let (wire1, wire2) = (wireToPoints puzzleInput1, wireToPoints puzzleInput2)
  -- let (wire1, wire2) = (wireToPoints example1, wireToPoints example2)
  in  minimum . map manhattanDistance $ filter (`elem` wire2) wire1

index :: [a] -> [(a, Int)]
index xs = zip xs [1 ..]

day3Part2 :: Int
day3Part2 = undefined
