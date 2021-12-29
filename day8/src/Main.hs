import qualified Data.Bifunctor as Bifunctor
import Data.List (findIndex, (\\))
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  testInput <- readInput "day8/test_input"
  input <- readInput "day8/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 26"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 321"
  print $ "Test input: " ++ show (secondProblem testInput) ++ " == 61229"
  print $ "Problem input: " ++ show (secondProblem input) ++ " == 1028926"
  where
    readInput file = map (separateByBar . words) . lines <$> readFile file
    separateByBar = Bifunctor.second tail . break (== "|")

firstProblem :: [([String], [String])] -> Int
firstProblem = sum . map (length . filterUniqueSegmentNumber . map length . snd)
  where
    filterUniqueSegmentNumber = filter hasUniqueSegmentNumber
    -- In some cases the number of segments used is unique and can identify the number
    hasUniqueSegmentNumber 2 = True -- 2 segments -> 1
    hasUniqueSegmentNumber 4 = True -- 4 segments -> 4
    hasUniqueSegmentNumber 3 = True -- 3 segments -> 7
    hasUniqueSegmentNumber 7 = True -- 7 segments -> 8
    hasUniqueSegmentNumber _ = False

secondProblem :: [([String], [String])] -> Int
secondProblem = sum . map decodeSegments

decodeSegments :: ([String], [String]) -> Int
decodeSegments =
  toInt . catMaybes . decodeOutput . Bifunctor.first identifyNumbers
  where
    toInt it = read (foldl1 (++) $ map show it)
    decodeOutput (numbers, output) =
      map ((`findIndex` numbers) . isSetEqual) output
    isSetEqual a b = b \\ a == "" && a \\ b == ""

identifyNumbers :: [String] -> [String]
identifyNumbers numbers = [zero, one, two, three, four, five, six, seven, eight, nine]
  where
    one = getElementWithLength 2 numbers
    four = getElementWithLength 4 numbers
    seven = getElementWithLength 3 numbers
    eight = getElementWithLength 7 numbers
    six = head . filterByDifference 1 one $ filterByLength 6 numbers
    nine = head . filterByDifference 0 four $ filterByLength 6 numbers
    zero = head . filter (\n -> (n /= six) && (n /= nine)) $ filterByLength 6 numbers
    three = head . filterByDifference 0 one $ filterByLength 5 numbers
    two = head . filterByDifference 2 four $ filterByLength 5 numbers
    five = head . filterByDifference 1 six $ filterByLength 5 numbers
    getElementWithLength len = head . filterByLength len
    filterByLength len = filter ((== len) . length)

-- Works by calculating set difference between the second argument
-- and each string in the third
filterByDifference :: Int -> String -> [String] -> [String]
filterByDifference diff original = filter ((== diff) . length . (original \\))