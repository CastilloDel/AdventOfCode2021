import Data.List (group, sort)
import Debug.Trace (trace)

timeToReproduce = 6

firstTimeToReproduce = 2 + timeToReproduce

main :: IO ()
main = do
  numbers <- map read . splitByCommas . head . lines <$> readFile "day6/input"
  print $ problem 80 numbers -- First problem -> 80 days
  print $ problem 256 numbers -- Second problem -> 256 days
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
  map ((\a -> a - 1) . length) . group . sort . (++ [0 .. firstTimeToReproduce])

advanceDay :: [Int] -> [Int]
advanceDay lanternfishes =
  map (calculateNewDay lanternfishes) [0 .. firstTimeToReproduce]

calculateNewDay :: [Int] -> Int -> Int
calculateNewDay lanternfishes val
  | val == firstTimeToReproduce = head lanternfishes
  | val == timeToReproduce =
    (lanternfishes !! (firstTimeToReproduce - 1)) + head lanternfishes
  | otherwise = lanternfishes !! (val + 1)