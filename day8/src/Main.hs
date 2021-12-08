import qualified Data.Bifunctor

-- usedSegments !! 2 equals the number of segments
-- used by a two in a seven segmen display
usedSegments = [6, 2, 5, 5, 4, 5, 6, 3, 7, 6]

main :: IO ()
main = do
  testInput <- readInput "day8/test_input"
  input <- readInput "day8/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 26"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 321"
  where
    readInput file = map (separateByBar . words) . lines <$> readFile file
    separateByBar = Data.Bifunctor.second tail . break (== "|")

firstProblem :: [([String], [String])] -> Int
firstProblem = sum . map (length . filterUniqueSegmentNumber . map length . snd)
  where
    filterUniqueSegmentNumber = filter hasUniqueSegmentNumber
    hasUniqueSegmentNumber num
      | num == usedSegments !! 1 = True
      | num == usedSegments !! 4 = True
      | num == usedSegments !! 7 = True
      | num == usedSegments !! 8 = True
      | otherwise = False