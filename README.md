# Advent of Code 2019 in Haskell

I saw [this Scala meet-up](http://www.lsug.co.uk/workshop/kata/scala/functional/typelevel/2020/03/06/workshop-coding-with-cats.html) where they were going to tackle the first 3 [Advent of Code](https://adventofcode.com/) challenges using FP in Scala, and thought it would be a good exercise to try it in Haskell.

## Running it

Requires `stack`. Do this:

```
> stack build && stack exec advent-of-code-exe
# lots of stack output ...
Day 1 part 1: 3471229
Day 1 part 2: 5203967
Day 2 part 1: 3306701
Day 2 part 2: 7621
Day 3 part 1: 399
Day 3 part 2: 15678
```

Warning: Day 3 is slow (~13 minutes on this laptop)

## Thoughts etc.

Some fairly gnarly-looking functions in Day 3, e.g. `getCrossingDistances`:

```haskell
getCrossingDistances :: [(Point, Int)] -> [(Point, Int)] -> [Int]
getCrossingDistances wire1 wire2 = foldl (\acc w -> f w wire2 ++ acc) [] wire1
 where
  f (point1, steps1) wire2 = if null g then [] else [steps1 + minimum g]
    where g = map snd (filter (\(point2, steps2) -> point1 == point2) wire2)
```

How to make this more readable? Pull out `f` and `g` from those nested `where`
blocks? Give them better names? It's hard naming functions that deal with tuples
though... e.g.`f` deals with a tuple of type `(Point, Int)`, containing a point
on a wire, and the number steps taken to get to that point from the origin. It
returns a list containing a single integer representing the shortest number of
steps taken to get from the origin to the point along both wires, if the point
is a crossing point, otherwise it returns an empty list.  Hard to give a good
name to that! This is a problem I often have writing FP code it seems hard to
break algorithms down into functions which are simple enough to be named easily
- I wonder if there's something I'm missing...
