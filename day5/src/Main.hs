{-# LANGUAGE TupleSections #-}

import Data.Char (isDigit)
import Data.List (group, sort)
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, satisfy, string)

main :: IO ()
main = do
  testLines <- map parseLine' . lines <$> readFile "day5/test_input"
  lines <- map parseLine' . lines <$> readFile "day5/input"
  print $ "Test input: " ++ show (firstProblem testLines) ++ " == 5"
  print $ "Problem input: " ++ show (firstProblem lines) ++ " == 5690"
  print $ "Test input: " ++ show (secondProblem testLines) ++ " == 12"
  print $ "Problem input: " ++ show (secondProblem lines) ++ " == 17741"
  where
    parseLine' = fst . last . readP_to_S parseLine

type Point = (Int, Int)

type Line = (Point, Point)

parsePoint :: ReadP Point
parsePoint = do
  x <- many1 $ satisfy isDigit
  char ','
  y <- many1 $ satisfy isDigit
  return (read x, read y)

parseLine :: ReadP Line
parseLine = do
  p1 <- parsePoint
  string " -> "
  p2 <- parsePoint
  return (p1, p2)

pointsFromStraightLine :: Line -> [Point]
pointsFromStraightLine ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) $ makeList y1 y2
  | y1 == y2 = map (,y1) $ makeList x1 x2
  | otherwise = [] :: [Point]

pointsFromDiagonalLine :: Line -> [Point]
pointsFromDiagonalLine ((x1, y1), (x2, y2))
  | abs (x1 - x2) == abs (y1 - y2) = zip (makeList x1 x2) (makeList y1 y2)
  | otherwise = [] :: [Point]

pointsFromLine :: Line -> [Point]
pointsFromLine line = pointsFromStraightLine line ++ pointsFromDiagonalLine line

makeList :: (Enum a, Ord a) => a -> a -> [a]
makeList a b = if a > b then reverse [b .. a] else [a .. b]

firstProblem :: [Line] -> Int
firstProblem = length . getRepeatedPoints . concatMap pointsFromStraightLine

secondProblem :: [Line] -> Int
secondProblem = length . getRepeatedPoints . concatMap pointsFromLine

getRepeatedPoints :: [Point] -> [Point]
getRepeatedPoints = map head . filter isRepeated . group . sort
  where
    isRepeated group = length group >= 2