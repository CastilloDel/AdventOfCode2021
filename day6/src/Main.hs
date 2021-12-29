import Data.List (group, sort)

timeToReproduce = 6

firstTimeToReproduce = 2 + timeToReproduce

main :: IO ()
main = do
  testNumbers <- map read . splitByCommas . head . lines <$> readFile "day6/test_input"
  numbers <- map read . splitByCommas . head . lines <$> readFile "day6/input"
  -- First problem -> 80 days
  print $ "Test input: " ++ show (problem 80 testNumbers) ++ " == 5934"
  print $ "Problem input: " ++ show (problem 80 numbers) ++ " == 362666"
  -- Second problem -> 256 days
  print $ "Test input: " ++ show (problem 256 testNumbers) ++ " == 26984457539"
  print $ "Problem input: " ++ show (problem 256 numbers) ++ " == 1640526601595"
  where
    splitByCommas = words . map (\a -> if a == ',' then ' ' else a)

-- Counts how many fishes there will be after n days
problem :: Int -> [Int] -> Int
problem days = sum . advanceNDays days . countOcurrences
  where
    advanceNDays n = (!! n) . iterate advanceDay

-- Initializes a list of length (firstTimeToReproduce + 1) in which each
-- position corresponds with the number of values equal to the index
-- 2,2,3,1,0,5 -> [1,1,2,1,0,1,0,0,0]
countOcurrences :: [Int] -> [Int]
countOcurrences =
  -- We add one of each type of fish to ensure the list gets properly initialized
  map ((+ (- 1)) . length) . group . sort . (++ [0 .. firstTimeToReproduce])

advanceDay :: [Int] -> [Int]
advanceDay lanternfishes =
  map (calculateNewDay lanternfishes) [0 .. firstTimeToReproduce]

calculateNewDay :: [Int] -> Int -> Int
calculateNewDay lanternfishes val
  | val == firstTimeToReproduce = head lanternfishes
  | val == timeToReproduce =
    (lanternfishes !! (firstTimeToReproduce - 1)) + head lanternfishes
  | otherwise = lanternfishes !! (val + 1)