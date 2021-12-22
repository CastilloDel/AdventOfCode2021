import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, intersection, map, size, toList, union)
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, satisfy)

main = do
  testBeacons <- readInput "day19/test_input"
  beacons <- readInput "day19/input"
  print $ "Test input: " ++ show (firstProblem testBeacons) ++ " == 79"
  print $ "Problem input: " ++ show (firstProblem beacons) ++ " == 459"
  print $ "Test input: " ++ show (firstProblem testBeacons) ++ " == 3621"
  print $ "Problem input: " ++ show (secondProblem beacons) ++ " == 19130"
  where
    readInput file =
      toCoordinates . splitOn [""] . filterTitles . lines <$> readFile file
    toCoordinates = map (Set.fromList . map parseCoordinates')
    parseCoordinates' = fst . last . readP_to_S parseCoordinates
    filterTitles = filter (not . startsWith "---")

startsWith :: String -> String -> Bool
startsWith pattern s = take (length pattern) s == pattern

type Beacons = Set Point

data Point = Point {x :: Int, y :: Int, z :: Int} deriving (Show, Eq, Ord)

parseCoordinates :: ReadP Point
parseCoordinates = do
  n1 <- parseNumber
  char ','
  n2 <- parseNumber
  char ','
  Point n1 n2 <$> parseNumber

parseNumber :: ReadP Int
parseNumber = do
  number <- many1 $ satisfy (\a -> isDigit a || a == '-')
  return $ read number

addCoordinates :: Point -> Point -> Point
addCoordinates (Point x1 y1 z1) (Point x2 y2 z2) =
  Point (x1 + x2) (y1 + y2) (z1 + z2)

subtractCoordinates :: Point -> Point -> Point
subtractCoordinates (Point x1 y1 z1) (Point x2 y2 z2) =
  Point (x1 - x2) (y1 - y2) (z1 - z2)

firstProblem :: [Beacons] -> Int
firstProblem (origin : rest) = Set.size $ fst $ discoverBeacons origin [] rest
firstProblem _ = error "Invalid input"

discoverBeacons :: Beacons -> [Point] -> [Beacons] -> (Beacons, [Point])
discoverBeacons known scanners unknown =
  maybe (known, scanners) keepDiscovering identification
  where
    keepDiscovering (new, old, newScanner) =
      discoverBeacons
        (Set.union new known)
        (newScanner : scanners)
        (filter (/= old) unknown)
    identification = foldMaybes $ map (identifyBeacons known) unknown

identifyBeacons :: Beacons -> Beacons -> Maybe (Beacons, Beacons, Point)
identifyBeacons origin other =
  foldMaybes $ map checkIntersection orientations
  where
    checkIntersection (orientation, shift) =
      if Set.size (Set.intersection origin orientation) >= 12
        then Just (orientation, other, shift)
        else Nothing
    orientations = concatMap applyShifts $ getAllOrientations other
    applyShifts other =
      [(Set.map (addCoordinates shift) other, shift) | shift <- getShifts other]
    getShifts other =
      [ a `subtractCoordinates` b
        | a <- Set.toList origin,
          b <- Set.toList other
      ]

foldMaybes :: [Maybe a] -> Maybe a
foldMaybes = foldl (<|>) Nothing

getAllOrientations :: Beacons -> [Beacons]
getAllOrientations beacons =
  [ Set.map (f . g . h . i) beacons
    | f <- [id, n1],
      g <- [id, n2],
      h <- [id, n3],
      i <- [id, s132, s213, s231, s312, s321]
  ]
  where
    n1 (Point a b c) = Point (- a) b c
    n2 (Point a b c) = Point a (- b) c
    n3 (Point a b c) = Point a b (- c)
    s132 (Point a b c) = Point a c b
    s213 (Point a b c) = Point b a c
    s231 (Point a b c) = Point b c a
    s312 (Point a b c) = Point c a b
    s321 (Point a b c) = Point c b a

secondProblem :: [Beacons] -> Int
secondProblem (origin : rest) = maximum scannerDistances
  where
    scannerDistances = [a `getManhattanDistance` b | a <- scanners, b <- scanners]
    scanners = snd $ discoverBeacons origin [] rest
secondProblem _ = error "Invalid input"

getManhattanDistance :: Point -> Point -> Int
getManhattanDistance (Point x1 y1 z1) (Point x2 y2 z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)