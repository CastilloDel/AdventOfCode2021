import Data.List (sort)

main :: IO ()
main = do
  testNumbers <- map read . splitByCommas . head . lines <$> readFile "day7/test_input"
  numbers <- map read . splitByCommas . head . lines <$> readFile "day7/input"
  print $ firstProblem testNumbers == 37
  print $ firstProblem numbers
  where
    splitByCommas = words . map (\a -> if a == ',' then ' ' else a)

firstProblem :: [Int] -> Int
firstProblem positions = sum $ map (\a -> abs $ a - center) positions
  where
    center = sort positions !! (length positions `div` 2)
