import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Maybe (fromJust, isNothing)
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, satisfy, (+++))

main = do
  testSnailNums <- readInput "day18/test_input"
  snailNums <- readInput "day18/input"
  print $ "Test input: " ++ show (firstProblem testSnailNums) ++ " == 4140"
  print $ "Problem input: " ++ show (firstProblem snailNums) ++ " == *"
  print $ "Test input: " ++ show (secondProblem testSnailNums) ++ " == 3393"
  print $ "Problem input: " ++ show (secondProblem snailNums) ++ " == 4701"
  where
    readInput file =
      map (fst . head . readP_to_S parseSnailNumber) . lines <$> readFile file

data SnailNumber = Num Int | Pair SnailNumber SnailNumber deriving (Show, Eq)

type FlattenedSnailNumber = [(Depth, Int)]

type Depth = Int

parseSnailNumber :: ReadP SnailNumber
parseSnailNumber = parseSnailNum +++ parseSnailPair

parseSnailNum :: ReadP SnailNumber
parseSnailNum = do
  num <- many1 $ satisfy isDigit
  return $ Num (read num :: Int)

parseSnailPair :: ReadP SnailNumber
parseSnailPair = do
  char '['
  snail1 <- parseSnailNumber
  char ','
  snail2 <- parseSnailNumber
  char ']'
  return $ Pair snail1 snail2

flattenSnailNumber :: SnailNumber -> FlattenedSnailNumber
flattenSnailNumber = flattenWithDepth 0
  where
    flattenWithDepth depth (Num num) = [(depth, num)]
    flattenWithDepth depth (Pair snail1 snail2) =
      flattenWithDepth newDepth snail1
        ++ flattenWithDepth newDepth snail2
      where
        newDepth = depth + 1

unflattenSnailNumber :: FlattenedSnailNumber -> SnailNumber
unflattenSnailNumber = fst . unflattenWithDepth 0
  where
    unflattenWithDepth depth list@((depth', n) : rest)
      | depth == depth' = (Num n, rest)
      | otherwise = (Pair snail1 snail2, rest'')
      where
        (snail1, rest') = unflattenWithDepth newDepth list
        (snail2, rest'') = unflattenWithDepth newDepth rest'
        newDepth = depth + 1
    unflattenWithDepth _ _ = error "Error while unflattening a snail number"

reduceSnailNumber :: SnailNumber -> SnailNumber
reduceSnailNumber = unflattenSnailNumber . reduce' . flattenSnailNumber
  where
    reduce' n = maybe n reduce' $ reduceStep n

reduceStep :: FlattenedSnailNumber -> Maybe FlattenedSnailNumber
reduceStep snail = explode snail <|> split snail

explode :: FlattenedSnailNumber -> Maybe FlattenedSnailNumber
explode ((5, _) : (5, c) : (d4, d) : rest) =
  Just ((4, 0) : (d4, c + d) : rest)
explode ((d1, a) : (5, b) : (5, c) : (d4, d) : rest) =
  Just ((d1, a + b) : (4, 0) : (d4, c + d) : rest)
explode [(d1, a), (5, b), (5, _)] =
  Just [(d1, a + b), (4, 0)]
explode (pair : rest) = (pair :) <$> explode rest
explode _ = Nothing

split :: FlattenedSnailNumber -> Maybe FlattenedSnailNumber
split ((depth, val) : rest)
  | val > 9 = Just ((newDepth, firstVal) : (newDepth, secondVal) : rest)
  | otherwise = ((depth, val) :) <$> split rest
  where
    newDepth = depth + 1
    firstVal = val `div` 2
    secondVal = val `div` 2 + val `mod` 2
split _ = Nothing

addSnailNumbers :: SnailNumber -> SnailNumber -> SnailNumber
addSnailNumbers a b = reduceSnailNumber (Pair a b)

getMagnitude :: SnailNumber -> Int
getMagnitude (Num val) = val
getMagnitude (Pair snail1 snail2) =
  3 * getMagnitude snail1 + 2 * getMagnitude snail2

firstProblem :: [SnailNumber] -> Int
firstProblem = getMagnitude . foldl1 addSnailNumbers

secondProblem :: [SnailNumber] -> Int
secondProblem snailNumbers = maximum $ map getMagnitude possibleResults
  where
    possibleResults =
      concatMap (\a -> map (addSnailNumbers a) snailNumbers) snailNumbers
