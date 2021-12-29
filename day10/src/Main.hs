import Data.Either (lefts, rights)
import Data.List (sort)
import qualified Data.Map as Map (fromList, lookup, (!))

pointsForIllegalChar = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

characterPairings = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

pointsForCompletion = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

main :: IO ()
main = do
  testInput <- readInput "day10/test_input"
  input <- readInput "day10/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 26397"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 288291"
  print $ "Test input: " ++ show (secondProblem testInput) ++ " == 288957"
  print $ "Problem input: " ++ show (secondProblem input) ++ " == 820045242"
  where
    readInput file = lines <$> readFile file

firstProblem :: [String] -> Int
firstProblem = sum . map (pointsForIllegalChar Map.!) . rights . map checkLineError

checkLineError :: String -> Either String Char
checkLineError = checkLineError' ""
  where
    checkLineError' stack [] = Left stack
    checkLineError' (c1 : stack) (c2 : string)
      | c2 == characterPairings Map.! c1 = checkLineError' stack string
    checkLineError' stack (char : rest)
      | char `elem` ['(', '[', '{', '<'] = checkLineError' (char : stack) rest
      | char `elem` [')', ']', '}', '>'] = Right char
      | otherwise = error $ "Invalid character: " ++ [char]

secondProblem :: [String] -> Int
secondProblem lines = sort pointsList !! (length pointsList `div` 2)
  where
    pointsList = map getCompletionPoints $ lefts $ map checkLineError lines

getCompletionPoints :: String -> Int
getCompletionPoints = foldl calculateCharPoints 0 . map getClosingPair

calculateCharPoints :: Int -> Char -> Int
calculateCharPoints acc char = case Map.lookup char pointsForCompletion of
  Just val -> val + acc * 5
  Nothing -> error $ "Legal characters don't have puntuation: " ++ [char]

getClosingPair :: Char -> Char
getClosingPair char = case Map.lookup char characterPairings of
  Just val -> val
  Nothing -> error $ "Character without pairing: " ++ [char]
