import Data.List (sort, transpose)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, toList)

-- This solution was based on the one by Giacomo Cavalieri (https://github.com/giacomocavalieri/aoc-2021/blob/main/src/Days/Day24.hs)
main :: IO ()
main = do
  constants <- readInput "day24/input"
  print $ "Problem input: " ++ show (firstProblem constants) ++ " == 51939397989999"
  print $ "Problem input: " ++ show (secondProblem constants) ++ " == 11717131211195"
  where
    readInput file = parseInput . lines <$> readFile file
    parseInput = combine . transpose . map getConstants . chunksOf 18
    combine list = zipWith keepOne (head list) (last list)
    keepOne x y = if x < 0 then x else y

getConstants :: [String] -> [Int]
getConstants lines = [getVal 5, getVal 15]
  where
    getVal lineNumber = read $ drop 6 $ lines !! lineNumber

firstProblem :: [Int] -> Int
firstProblem = toNumber . solveConstraints solver . findConstraints
  where
    -- Solve the constraint using the highest values possible
    solver offset = if offset >= 0 then (9, 9 - offset) else (9 + offset, 9)

secondProblem :: [Int] -> Int
secondProblem = toNumber . solveConstraints solver . findConstraints
  where
    -- Solve the constraint using the lowest values possible
    solver offset = if offset >= 0 then (offset + 1, 1) else (1, 1 - offset)

type Digit = Int

type Offset = Int

data Constraint = Constraint Digit Digit Offset deriving (Show)

findConstraints :: [Int] -> [Constraint]
findConstraints = snd . foldl extractConstraint ([], []) . zip [1 ..]
  where
    extractConstraint (stack, cs) (digit, n)
      | n >= 0 = ((digit, n) : stack, cs)
      | otherwise = (tail stack, Constraint digit digit' (n + m) : cs)
      where
        (digit', m) = head stack

solveConstraints :: (Offset -> (Int, Int)) -> [Constraint] -> Map Digit Int
solveConstraints _ [] = Map.empty
solveConstraints solver ((Constraint digit1 digit2 offset) : rest) = m'
  where
    (val1, val2) = solver offset
    m = solveConstraints solver rest
    m' = Map.insert digit1 val1 $ Map.insert digit2 val2 m

toNumber :: Map Digit Int -> Int
toNumber = foldl (\total (_, digit) -> total * 10 + digit) 0 . sort . Map.toList