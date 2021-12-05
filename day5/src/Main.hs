import Data.List (group, sort)
import Debug.Trace (trace)

main :: IO ()
main = do
  lines <- map parseLine . lines <$> readFile "day5/input"
  print $ firstProblem lines

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

pointsFromLine :: Line -> [Point]
pointsFromLine (Line (Point (x1, y1), Point (x2, y2)))
  | x1 == x2 = map (\y -> Point (x1, y)) $ makeList y1 y2
  | y1 == y2 = map (\x -> Point (x, y1)) $ makeList x1 x2
  | otherwise = [] :: [Point]
  where
    makeList = \a b -> if a > b then [b .. a] else [a .. b]

lineFromList :: [Point] -> Line
lineFromList [point1, point2] = Line (point1, point2)
lineFromList _ = error "Couldn't convert to Point"

firstProblem :: [Line] -> Int
firstProblem = length . repeatedPointsFromLines

repeatedPointsFromLines :: [Line] -> [Point]
repeatedPointsFromLines lines =
  map head $ filterNonRepeated $ group $ sort totalPointList
  where
    filterNonRepeated = filter (\group -> length group >= 2)
    totalPointList = concatMap pointsFromLine lines