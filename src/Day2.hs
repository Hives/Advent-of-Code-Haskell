module Day2 where

import           Day2Input

replaceNthPos :: Int -> Int -> [Int] -> [Int]
replaceNthPos _ _ [] = error $ "Ran out of list elements"
replaceNthPos n newVal (a : as)
  | n == 0    = newVal : as
  | otherwise = a : replaceNthPos (n - 1) newVal as

readPos :: Int -> [Int] -> Int
readPos _ [] = error $ "Ran out of list elements"
readPos n (a : as) | n == 0    = a
                   | otherwise = readPos (n - 1) as

readReferredPos :: Int -> [Int] -> Int
readReferredPos _ []      = error $ "Ran out of list elements"
readReferredPos n program = let m = readPos n program in readPos m program

execute :: Int -> [Int] -> [Int]
execute n program
  | readPos n program == 1
  -- opcode 1: addition
  = let (input1, input2, target, newProgram) =
            ( readReferredPos (n + 1) program
            , readReferredPos (n + 2) program
            , readPos (n + 3) program
            , replaceNthPos target (input1 + input2) program
            )
    in  execute (n + 4) newProgram
  | readPos n program == 2
  -- opcode 2: multiplication
  = let (input1, input2, target, newProgram) =
            ( readReferredPos (n + 1) program
            , readReferredPos (n + 2) program
            , readPos (n + 3) program
            , replaceNthPos target (input1 * input2) program
            )
    in  execute (n + 4) newProgram
  | readPos n program == 99
  -- opcode 99: halt
  = program

prepareInput :: Int -> Int -> [Int]
prepareInput noun verb =
  replaceNthPos 1 noun (replaceNthPos 2 verb gravityAssistProgram)

getOutputOfExecution :: Int -> Int -> Int
getOutputOfExecution noun verb = head (execute 0 (prepareInput noun verb))

day2Part1 :: Int
day2Part1 = getOutputOfExecution 12 2

manyNounsAndVerbs :: [(Int, Int)]
manyNounsAndVerbs = f [0 .. 99] []
 where
  f []       acc = acc
  f (x : xs) acc = f xs (acc ++ map (\n -> (n, x)) [0 .. 99])

desiredOutput = 19690720

day2Part2 :: Int
day2Part2 =
  let
    (manyExecutions, desiredExecution, noun, verb) =
      ( map (\(noun, verb) -> ((noun, verb), getOutputOfExecution noun verb))
            manyNounsAndVerbs
      , fst $ head $ filter (\(_, output) -> output == desiredOutput)
                            manyExecutions
      , fst desiredExecution
      , snd desiredExecution
      )
  in  100 * noun + verb
