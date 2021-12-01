main :: IO ()
main = do
  lines <- map (\a -> read a :: Int) . lines <$> readFile "day1/day1_input"
  print $ length $ filter (uncurry (<)) $ zip lines (tail lines)
