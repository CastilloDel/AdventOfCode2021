import Data.Char (isLower)
import Data.List (nub)
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
  print $ "Test input: " ++ show (secondProblem testInput) ++ " == 36"
  print $ "Problem input: " ++ show (secondProblem input) ++ " == 120506"
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

type Path = [Cave]

firstProblem :: CaveConections -> Int
firstProblem = problem caveFilter
  where
    caveFilter path cave = not $ isSmall cave && elem cave path

secondProblem :: CaveConections -> Int
secondProblem = problem caveFilter
  where
    caveFilter path cave =
      let visitedSmallTwice = (\a -> length (nub a) /= length a) $ filter isSmall path
       in if visitedSmallTwice
            then not $ isSmall cave && elem cave path
            else cave /= "start"

problem :: (Path -> Cave -> Bool) -> CaveConections -> Int
problem predicate = length . getPaths predicate

getPaths :: (Path -> Cave -> Bool) -> CaveConections -> [Path]
getPaths predicate connections = getPaths' ["start"]
  where
    getPaths' path@("end" : _) = [reverse path]
    getPaths' path =
      let possibleMovements = filterByPredicate path $ connections ! head path
       in if null possibleMovements
            then []
            else concatMap (getPaths' . (: path)) possibleMovements
    filterByPredicate path = filter $ predicate path
