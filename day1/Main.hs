main :: IO ()
main = do
  lines <- map (\a -> read a :: Int) . lines <$> readFile "day1/day1_input"
  print $ firstProblem lines
  print $ secondProblem lines

-- Counts how many increments there are in the list
firstProblem :: [Int] -> Int
firstProblem list = length $ filter (uncurry (<)) $ zip list (tail list)

-- Counts how many increments there are, but doesn't directly use the
-- contents of the list, instead it gets a new list with the summed values
-- of sucessive windows of size three
secondProblem :: [Int] -> Int
secondProblem list =
  let windows = zip3 list (tail list) (tail $ tail list)
      window_values = map (sum . tripleToList) windows
   in length $ filter (uncurry (<)) $ zip window_values (tail window_values)

tripleToList :: (a, a, a) -> [a]
tripleToList (x, y, z) = [x, y, z]
