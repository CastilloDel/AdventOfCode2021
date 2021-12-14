import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Map as M (Map, empty, insertWith, (!))

{-
Some considerations about the input:
* There can't be two adjacent big caves
* There must be a cave called "start" and another one called "end"
-}

main :: IO ()
main = do
  testInput <- readInput "day12/test_input"
  input <- readInput "day12/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 10"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 3738"
  where
    readInput file = foldl foldLine M.empty . lines <$> readFile file
    foldLine acc = insertNew acc . splitOn "-"
    insertNew map (cave1 : cave2 : _) =
      insertWith (++) cave1 [cave2] $ insertWith (++) cave2 [cave1] map
    insertNew _ out = error $ "Invalid split: " ++ show out

type Cave = String

isSmall :: Cave -> Bool
isSmall = isLower . head

type CaveConections = Map Cave [Cave]

firstProblem :: CaveConections -> Int
firstProblem = length . getPaths

getPaths :: CaveConections -> [[String]]
getPaths connections = getPaths' ["start"]
  where
    getPaths' path@("end" : _) = [reverse path]
    getPaths' path =
      let possibleMovements = filterSmall path $ connections ! head path
       in if null possibleMovements
            then []
            else concatMap (getPaths' . (: path)) possibleMovements
    filterSmall path = filter $ not . (\cave -> isSmall cave && elem cave path)