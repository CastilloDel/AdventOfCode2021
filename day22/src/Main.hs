{-# LANGUAGE TupleSections #-}

import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set (difference, empty, filter, fromList, size, union)
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, satisfy, string, (+++))

main :: IO ()
main = do
  testRules <- readInput "day22/test_input"
  rules <- readInput "day22/input"
  print $ "Test input: " ++ show (firstProblem testRules) ++ " == 39"
  print $ "Problem input: " ++ show (firstProblem rules) ++ " == 644257"
  where
    readInput file = map parseInput . lines <$> readFile file
    parseInput = fst . last . readP_to_S parseRule

data Rule = On Area | Off Area deriving (Show)

data Area = Area {x :: Limits, y :: Limits, z :: Limits} deriving (Show)

-- Must be (lowerLimit, upperLimit)
type Limits = (Int, Int)

type Point = (Int, Int, Int)

parseRule :: ReadP Rule
parseRule = parseOnRule +++ parseOffRule

parseOnRule :: ReadP Rule
parseOnRule = do
  string "on "
  On <$> parseArea

parseOffRule :: ReadP Rule
parseOffRule = do
  string "off "
  Off <$> parseArea

parseArea :: ReadP Area
parseArea = do
  l1 <- parseLimits
  char ','
  l2 <- parseLimits
  char ','
  Area l1 l2 <$> parseLimits

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

firstProblem :: [Rule] -> Int
firstProblem = Set.size . foldl foldIntoSet Set.empty
  where
    foldIntoSet set (On area) = Set.union set $ setFromArea area
    foldIntoSet set (Off area) = Set.difference set $ setFromArea area

setFromArea :: Area -> Set Point
setFromArea (Area (x1, x2) (y1, y2) (z1, z2))
  | isInsideInitializationArea =
    Set.fromList [(x, y, z) | x <- [x1 .. x2], y <- [y1 .. y2], z <- [z1 .. z2]]
  | otherwise = Set.empty
  where
    isInsideInitializationArea =
      x1 >= -50 && x2 <= 50 && y1 >= -50 && y2 <= 50 && z1 >= -50 && z2 <= 50