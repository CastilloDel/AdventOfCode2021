{-# LANGUAGE TupleSections #-}

import Data.Char (isDigit)
import Data.Maybe (fromJust, isJust)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as Set (difference, empty, filter, fromList, insert, map, size, union)
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, satisfy, string, (+++))

main :: IO ()
main = do
  testSteps <- readInput "day22/test_input"
  steps <- readInput "day22/input"
  print $ "Test input: " ++ show (firstProblem testSteps) ++ " == 474140"
  print $ "Problem input: " ++ show (firstProblem steps) ++ " == 644257"
  print $ "Test input: " ++ show (secondProblem testSteps) ++ " == 2758514936282235"
  print $ "Problem input: " ++ show (secondProblem steps) ++ " == 1235484513229032"
  where
    readInput file = map parseInput . lines <$> readFile file
    parseInput = fst . last . readP_to_S parseStep

data Step = Step StepType Cuboid deriving (Show, Eq)

data StepType = On | Off deriving (Show, Eq)

data Cuboid = Cuboid {x :: Limits, y :: Limits, z :: Limits}
  deriving (Show, Eq, Ord)

-- Must be (lowerLimit, upperLimit)
type Limits = (Int, Int)

parseStep :: ReadP Step
parseStep = parseOnStep +++ parseOffStep

parseOnStep :: ReadP Step
parseOnStep = do
  string "on "
  Step On <$> parseCuboid

parseOffStep :: ReadP Step
parseOffStep = do
  string "off "
  Step Off <$> parseCuboid

parseCuboid :: ReadP Cuboid
parseCuboid = do
  l1 <- parseLimits
  char ','
  l2 <- parseLimits
  char ','
  Cuboid l1 l2 <$> parseLimits

parseLimits :: ReadP Limits
parseLimits = do
  satisfy (const True)
  satisfy (const True)
  low <- parseNumber
  string ".."
  (low,) <$> parseNumber

parseNumber :: ReadP Int
parseNumber = do
  number <- many1 $ satisfy (\a -> isDigit a || a == '-')
  return $ read number

firstProblem :: [Step] -> Int
firstProblem = reboot . filterInitialization
  where
    filterInitialization = filter (isInside 50 50 . getCuboidFromStep)
    isInside min max (Cuboid (x1, x2) (y1, y2) (z1, z2)) =
      x1 >= -50 && x2 <= 50 && y1 >= -50 && y2 <= 50 && z1 >= -50 && z2 <= 50

secondProblem :: [Step] -> Int
secondProblem = reboot

reboot :: [Step] -> Int
reboot = count . foldl applyStep (Set.empty, Set.empty)
  where
    count (add, sub) = getCardinalitySum add - getCardinalitySum sub
    getCardinalitySum set = sum (Set.map getCardinality set)

type State = (MultiSet Cuboid, MultiSet Cuboid)

applyStep :: State -> Step -> State
applyStep (add, sub) (Step t cuboid)
  | t == On =
    ( Set.insert cuboid $ Set.union subIntersections add,
      Set.union addIntersections sub
    )
  | otherwise =
    ( Set.union subIntersections add,
      Set.union addIntersections sub
    )
  where
    addIntersections = filterNothing $ Set.map (getIntersection cuboid) add
    subIntersections = filterNothing $ Set.map (getIntersection cuboid) sub
    filterNothing = Set.map fromJust . Set.filter isJust

getCuboidFromStep :: Step -> Cuboid
getCuboidFromStep (Step _ area) = area

getCardinality :: Cuboid -> Int
getCardinality (Cuboid (x1, x2) (y1, y2) (z1, z2)) =
  getDist x1 x2 * getDist y2 y1 * getDist z2 z1
  where
    getDist a b = abs (a - b) + 1

getIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
getIntersection (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = do
  x <- getIntersection1D x1 x2
  y <- getIntersection1D y1 y2
  Cuboid x y <$> getIntersection1D z1 z2

getIntersection1D :: Limits -> Limits -> Maybe Limits
getIntersection1D (l1, u1) (l2, u2)
  | lower <= upper = Just (lower, upper)
  | otherwise = Nothing
  where
    lower = max l1 l2
    upper = min u1 u2