main :: IO ()
main = do
  lines <- map (\a -> read a :: Int) . lines <$> readFile "day1/day1_input"
  print $ length $ filter (uncurry (<)) $ zip lines (tail lines)
  let window_values = map (sum . tripleToList) $ zip3 lines (tail lines) (tail $ tail lines)
  print $ length $ filter (uncurry (<)) $ zip window_values (tail window_values)

tripleToList :: (a, a, a) -> [a]
tripleToList (x, y, z) = [x, y, z]
