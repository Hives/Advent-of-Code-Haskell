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

sectionToPoints :: String -> Point -> [Point]
sectionToPoints section (Point x y) = case section of
  ('R' : n) -> map (\i -> Point (x + i) y) [1 .. read n :: Int]
  ('U' : n) -> map (\i -> Point x (y + i)) [1 .. read n :: Int]
  ('L' : n) -> map (\i -> Point (x - i) y) [1 .. read n :: Int]
  ('D' : n) -> map (\i -> Point x (y - i)) [1 .. read n :: Int]

wireToPoints :: [String] -> [Point]
wireToPoints wire = f wire (Point 0 0) []
 where
  f [] _ acc = acc
  f (w : ws) (Point x y) acc =
    let newSection = sectionToPoints w (Point x y)
    in  f ws (last newSection) (acc ++ newSection)

manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = abs x + abs y

day3Part1 :: Int
day3Part1 =
  let (wire1, wire2) = (wireToPoints puzzleInput1, wireToPoints puzzleInput2)
  in  minimum . map manhattanDistance $ filter (`elem` wire2) wire1

index :: [a] -> [(a, Int)]
index xs = zip xs [1 ..]

getCrossingDistances :: [(Point, Int)] -> [(Point, Int)] -> [Int]
getCrossingDistances wire1 wire2 = foldl (\acc w -> f w wire2 ++ acc) [] wire1
 where
  f (point1, steps1) wire2 = if null g then [] else [steps1 + minimum g]
    where g = map snd (filter (\(point2, steps2) -> point1 == point2) wire2)

day3Part2 :: Int
day3Part2 =
  let (wire1, wire2) =
          (index . wireToPoints $ example21, index . wireToPoints $ example22)
  in  minimum (getCrossingDistances wire1 wire2)

