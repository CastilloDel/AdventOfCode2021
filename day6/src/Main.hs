main :: IO ()
main = do
  numbers <- map read . splitByCommas . head . lines <$> readFile "day6/input"
  print $ firstProblem numbers
  where
    splitByCommas = words . map (\a -> if a == ',' then ' ' else a)

firstProblem :: [Int] -> Int
firstProblem = length . (!! 80) . iterate advanceDay

advanceDay :: [Int] -> [Int]
advanceDay = uncurry (++) . foldr calculateNewDay ([], [])
  where
    timeToReproduce = 6
    firstTimeToReproduce = 2 + timeToReproduce
    calculateNewDay 0 (old, new) = (timeToReproduce : old, firstTimeToReproduce : new)
    calculateNewDay val (old, new) = (val - 1 : old, new)
