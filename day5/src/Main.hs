import Data.List (group, sort)

main :: IO ()
main = do
  lines <- map parseLine . lines <$> readFile "day5/input"
  print $ firstProblem lines
  print $ secondProblem lines

newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

parsePosition :: String -> Point
parsePosition = pointFromList . map read . separateCoordinates
  where
    separateCoordinates = words . map (\a -> if a == ',' then ' ' else a)

pointFromList :: [Int] -> Point
pointFromList [x, y] = Point (x, y)
pointFromList lines = error "Couldn't convert to Point"

newtype Line = Line (Point, Point) deriving (Show)

parseLine :: String -> Line
parseLine = lineFromList . map parsePosition . filter (/= "->") . words

pointsFromStraightLine :: Line -> [Point]
pointsFromStraightLine (Line (Point (x1, y1), Point (x2, y2)))
  | x1 == x2 = map (\y -> Point (x1, y)) $ makeList y1 y2
  | y1 == y2 = map (\x -> Point (x, y1)) $ makeList x1 x2
  | otherwise = [] :: [Point]

pointsFromDiagonalLine :: Line -> [Point]
pointsFromDiagonalLine (Line (Point (x1, y1), Point (x2, y2)))
  | abs (x1 - x2) == abs (y1 - y2) =
    zipWith (curry Point) (makeList x1 x2) (makeList y1 y2)
  | otherwise = [] :: [Point]

pointsFromLine :: Line -> [Point]
pointsFromLine line = pointsFromStraightLine line ++ pointsFromDiagonalLine line

makeList :: (Enum a, Ord a) => a -> a -> [a]
makeList a b = if a > b then reverse [b .. a] else [a .. b]

lineFromList :: [Point] -> Line
lineFromList [point1, point2] = Line (point1, point2)
lineFromList _ = error "Couldn't convert to Point"

firstProblem :: [Line] -> Int
firstProblem = length . getRepeatedPoints . concatMap pointsFromStraightLine

secondProblem :: [Line] -> Int
secondProblem = length . getRepeatedPoints . concatMap pointsFromLine

getRepeatedPoints :: [Point] -> [Point]
getRepeatedPoints = map head . filterNonRepeated . group . sort
  where
    filterNonRepeated = filter (\group -> length group >= 2)