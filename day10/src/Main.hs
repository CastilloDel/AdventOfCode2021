import qualified Data.Map as Map

pointsForIllegalChar = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

characterPairings = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

main :: IO ()
main = do
  testInput <- readInput "day10/test_input"
  input <- readInput "day10/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 26397"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 288291"
  where
    readInput file = lines <$> readFile file

firstProblem :: [String] -> Int
firstProblem = sum . map (checkPossibleErrorPoints . checkLineError)

checkLineError :: String -> Maybe Char
checkLineError = checkLineError' ""
  where
    checkLineError' _ [] = Nothing
    checkLineError' (c1 : stack) (c2 : string)
      | Just c2 == Map.lookup c1 characterPairings = checkLineError' stack string
    checkLineError' stack (char : rest)
      | char `elem` ['(', '[', '{', '<'] = checkLineError' (char : stack) rest
      | char `elem` [')', ']', '}', '>'] = Just char
      | otherwise = error $ "Invalid character: " ++ [char]

checkPossibleErrorPoints :: Maybe Char -> Int
checkPossibleErrorPoints Nothing = 0
checkPossibleErrorPoints (Just val) = case Map.lookup val pointsForIllegalChar of
  Just val -> val
  Nothing -> error "Legal characters don't have puntuation"
