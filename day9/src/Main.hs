import Data.Char (digitToInt)
import qualified Data.Map as Map

main :: IO ()
main = do
  testInput <- readInput "day9/test_input"
  input <- readInput "day9/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 15"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 496"
  where
    readInput file = caveMapFromMatrix . map (map digitToInt) . lines <$> readFile file

data CaveMap = CaveMap {height :: Int, width :: Int, contents :: Map.Map (Int, Int) Int}

caveMapFromMatrix :: [[Int]] -> CaveMap
caveMapFromMatrix matrix = CaveMap {height = height, width = width, contents = contents}
  where
    height = length matrix
    width = length $ head matrix
    contents = Map.fromList $ zip indexes $ concat matrix
    indexes = indexesFromSize height width

strictLookup :: (Int, Int) -> CaveMap -> Int
strictLookup key CaveMap {contents = contents} =
  case Map.lookup key contents of
    Just val -> val
    Nothing -> error $ "Couldn't find an element with key: " ++ show key

isValidPoint :: (Int, Int) -> CaveMap -> Bool
isValidPoint (i, j) CaveMap {height = height, width = width}
  | i < 0 || j < 0 || i >= height || j >= width = False
  | otherwise = True

isLowPoint :: (Int, Int) -> CaveMap -> Bool
isLowPoint index caveMap@CaveMap {height = height, width = width} =
  all (> value) adjacentValues
  where
    value = strictLookup index caveMap
    adjacentValues = map (`strictLookup` caveMap) adjacentIndexes
    adjacentIndexes = filter (`isValidPoint` caveMap) $ getAdjacentPositions index

getAdjacentPositions :: (Int, Int) -> [(Int, Int)]
getAdjacentPositions (i, j) = [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]

indexesFromSize :: Int -> Int -> [(Int, Int)]
indexesFromSize height width =
  concatMap ((`zip` [0 .. width - 1]) . replicate width) [0 .. height - 1]

firstProblem :: CaveMap -> Int
firstProblem caveMap@CaveMap {height = height, width = width} =
  sum $ map (+ 1) lowPoints
  where
    lowPoints = map (`strictLookup` caveMap) lowPointsIndexes
    lowPointsIndexes = filter (`isLowPoint` caveMap) indexes
    indexes = indexesFromSize height width