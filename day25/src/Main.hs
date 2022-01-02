{-# LANGUAGE TupleSections #-}

import Data.Map (Map, (!))
import qualified Data.Map as Map (fromList, mapKeys, member)
import Data.Maybe (isNothing, mapMaybe)

main :: IO ()
main = do
  testCucumbers <- readInput "day25/test_input"
  cucumbers <- readInput "day25/input"
  print $ "Problem input: " ++ show (firstProblem testCucumbers) ++ " == 58"
  print $ "Problem input: " ++ show (firstProblem cucumbers) ++ " == 513"
  where
    readInput file = parseInput . lines <$> readFile file

parseInput :: [String] -> Cucumbers
parseInput lines = Cucumbers m n cucumbersMap
  where
    m = length lines
    n = length $ head lines
    cucumbersMap = Map.fromList cucumbersWithPos
    cucumbersWithPos = mapMaybe cucumberOrNothing $ concat linesWithPos
    cucumberOrNothing (pos, c) = (pos,) <$> parseCucumber c
    linesWithPos =
      zipWith (\i line -> zip (map (i,) [0 .. n - 1]) line) [0 .. m -1] lines

parseCucumber :: Char -> Maybe CucumberType
parseCucumber '>' = Just East
parseCucumber 'v' = Just South
parseCucumber _ = Nothing

-- Cucumbers stores the size of the map and a map filled with the occupied positions
data Cucumbers = Cucumbers Int Int (Map Pos CucumberType) deriving (Eq, Show)

type Pos = (Int, Int)

data CucumberType = East | South deriving (Show, Eq, Ord)

firstProblem :: Cucumbers -> Int
firstProblem = countStepsUntilStop
  where
    countStepsUntilStop cucumbers =
      if cucumbers == newCucumbers
        then 1
        else 1 + countStepsUntilStop newCucumbers
      where
        newCucumbers = step cucumbers

step :: Cucumbers -> Cucumbers
step = partialStep South . partialStep East

partialStep :: CucumberType -> Cucumbers -> Cucumbers
partialStep typ (Cucumbers m n cucumberMap) = Cucumbers m n newCucumberMap
  where
    newCucumberMap = Map.mapKeys moveEast cucumberMap
    moveEast pos = if cucumber == typ then nextPos else pos
      where
        nextPos =
          if Map.member possibleNextPos cucumberMap then pos else possibleNextPos
        possibleNextPos = cucumberNextPosition m n pos cucumber
        cucumber = cucumberMap ! pos

cucumberNextPosition :: Int -> Int -> Pos -> CucumberType -> Pos
cucumberNextPosition m n (i, j) East = (i, (j + 1) `mod` n)
cucumberNextPosition m n (i, j) South = ((i + 1) `mod` m, j)