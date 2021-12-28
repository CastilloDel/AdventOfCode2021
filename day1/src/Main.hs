main :: IO ()
main = do
  testNumbers <- map read . lines <$> readFile "day1/test_input"
  numbers <- map read . lines <$> readFile "day1/input"
  print $ "Test input: " ++ show (firstProblem testNumbers) ++ " == 7"
  print $ "Problem input: " ++ show (firstProblem numbers) ++ " == 1298"
  print $ "Test input: " ++ show (secondProblem testNumbers) ++ " == 5"
  print $ "Problem input: " ++ show (secondProblem numbers) ++ " == 1248"

-- Counts how many increments there are in the list
firstProblem :: [Int] -> Int
firstProblem list = length $ filter (uncurry (<)) $ zip list (tail list)

-- Counts how many increments there are, but doesn't directly use the
-- contents of the list, instead it gets a new list with the summed values
-- of sucessive windows of size three
secondProblem :: [Int] -> Int
secondProblem list =
  length $ filter (uncurry (<)) $ zip window_values (tail window_values)
  where
    windows = zip3 list (tail list) (tail $ tail list)
    window_values = map (sum . tripleToList) windows

tripleToList :: (a, a, a) -> [a]
tripleToList (x, y, z) = [x, y, z]
