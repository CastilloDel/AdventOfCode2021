import Data.List (sort)

main :: IO ()
main = do
  testNumbers <- map read . splitByCommas . head . lines <$> readFile "day7/test_input"
  numbers <- map read . splitByCommas . head . lines <$> readFile "day7/input"
  print $ "Test input: " ++ show (firstProblem testNumbers == 37)
  print $ firstProblem numbers
  print $ "Test input: " ++ show (secondProblem testNumbers == 168)
  print $ secondProblem numbers
  where
    splitByCommas = words . map (\a -> if a == ',' then ' ' else a)

firstProblem :: [Int] -> Int
firstProblem positions = sum $ map (\a -> abs $ a - center) positions
  where
    -- The center in the first problem can be seen as the median
    center = sort positions !! (length positions `div` 2)

secondProblem :: [Int] -> Int
secondProblem positions = minimum (map (`calculateDeviationSum` positions) possibleCenters)
  where
    calculateDeviationSum center = sum . map (sumAdditional . abs . (+ (- center)))
    -- The center in the second problem can be seen as the mean
    -- We need to check both values because the actual fractional mean is between them
    -- Rounding won't work, because we are dealing with Integers and we lose some information,
    -- so the real mean might not be entirely accurate
    possibleCenters = [mean, mean + 1]
    mean = div (sum positions) (length positions)
    sumAdditional val = sum [1 .. val]