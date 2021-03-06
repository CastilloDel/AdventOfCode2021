import Data.List (transpose)
import Data.Maybe (isNothing, mapMaybe)

main :: IO ()
main = do
  (testNumbers, testTables) <- parseInput . lines <$> readFile "day4/test_input"
  (numbers, tables) <- parseInput . lines <$> readFile "day4/input"
  print $ "Test input: " ++ show (firstProblem testNumbers testTables) ++ " == 4512"
  print $ "Problem input: " ++ show (firstProblem numbers tables) ++ " == 38594"
  print $ "Test input: " ++ show (secondProblem testNumbers testTables) ++ " == 1924"
  print $ "Problem input: " ++ show (secondProblem numbers tables) ++ " == 21184"

newtype BingoTable = BingoTable [[Maybe Int]] deriving (Show)

parseInput :: [String] -> ([Int], [BingoTable])
parseInput input = (numbers, tables)
  where
    numbers = map read $ split ',' $ head input
    tables = parseBingoTables $ tail input

parseBingoTables :: [String] -> [BingoTable]
parseBingoTables = map parseBingoTable . split ""

parseBingoTable :: [String] -> BingoTable
parseBingoTable = BingoTable . map (map (Just . read) . words)

split :: (Eq a) => a -> [a] -> [[a]]
split c [] = [[]]
split c list = recursiveSplit $ break (== c) list
  where
    recursiveSplit (s1, []) = [s1]
    recursiveSplit ([], s2) = split c (tail s2)
    recursiveSplit (s1, s2) = s1 : split c (tail s2)

firstProblem :: [Int] -> [BingoTable] -> Maybe Int
firstProblem [] tables = Nothing
firstProblem (first : rest) tables =
  if null possibleBingos
    then firstProblem rest newTables
    else Just $ head possibleBingos * first
  where
    newTables = map (newNumber first) tables
    possibleBingos = mapMaybe calculateBingo newTables

newNumber :: Int -> BingoTable -> BingoTable
newNumber number (BingoTable table) = BingoTable $ map (map filterToNothing) table
  where
    filterToNothing (Just val)
      | val == number = Nothing
      | otherwise = Just val
    filterToNothing Nothing = Nothing

calculateBingo :: BingoTable -> Maybe Int
calculateBingo bingoTable@(BingoTable table)
  | checkBingo bingoTable = Just $ sum $ map sumMaybes table
  | otherwise = Nothing
  where
    sumMaybes = foldl sumMaybe 0
    sumMaybe val1 (Just val2) = val1 + val2
    sumMaybe val1 Nothing = val1

checkBingo :: BingoTable -> Bool
checkBingo (BingoTable table) = checkRow table || checkRow (transpose table)
  where
    checkRow = any (all isNothing)

secondProblem :: [Int] -> [BingoTable] -> Maybe Int
secondProblem [] tables = Nothing
secondProblem (first : rest) tables =
  if null incompleteTables
    then (* first) <$> calculateBingo (head newTables)
    else secondProblem rest incompleteTables
  where
    newTables = map (newNumber first) tables
    incompleteTables = filter (not . checkBingo) newTables