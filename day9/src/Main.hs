import Data.Char (digitToInt)
import Data.List (sort, (\\))
import qualified Data.Map as Map

main :: IO ()
main = do
  testInput <- readInput "day9/test_input"
  input <- readInput "day9/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 15"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 496"
  print $ "Test input: " ++ show (secondProblem testInput) ++ " == 1134"
  print $ "Problem input: " ++ show (secondProblem input) ++ " == 902880"
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

strictLookup :: CaveMap -> (Int, Int) -> Int
strictLookup CaveMap {contents = contents} key =
  case Map.lookup key contents of
    Just val -> val
    Nothing -> error $ "Couldn't find an element with key: " ++ show key

isValidPoint :: CaveMap -> (Int, Int) -> Bool
isValidPoint CaveMap {height = height, width = width} (i, j) =
  not $ i < 0 || j < 0 || i >= height || j >= width

isLowPoint :: CaveMap -> (Int, Int) -> Bool
isLowPoint caveMap@CaveMap {height = height, width = width} index =
  all (> value) adjacentValues
  where
    value = strictLookup caveMap index
    adjacentValues = map (strictLookup caveMap) adjacentIndexes
    adjacentIndexes = filter (isValidPoint caveMap) $ getAdjacentPositions index

getLowPoints :: CaveMap -> [(Int, Int)]
getLowPoints caveMap@CaveMap {height = height, width = width} =
  filter (isLowPoint caveMap) indexes
  where
    indexes = indexesFromSize height width

getBasin :: CaveMap -> (Int, Int) -> [(Int, Int)]
getBasin caveMap = getBasinIncomplete caveMap []

getBasinIncomplete :: CaveMap -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
getBasinIncomplete caveMap explored new =
  foldl (getBasinIncomplete caveMap) newExplored newPoints
  where
    newExplored = explored ++ newPoints
    newPoints = pointsInBasin \\ explored
    pointsInBasin = filter ((< 9) . strictLookup caveMap) possibleBasinPoints
    possibleBasinPoints = filter (isValidPoint caveMap) $ getAdjacentPositions new

getAdjacentPositions :: (Int, Int) -> [(Int, Int)]
getAdjacentPositions (i, j) = [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]

indexesFromSize :: Int -> Int -> [(Int, Int)]
indexesFromSize height width =
  concatMap ((`zip` [0 .. width - 1]) . replicate width) [0 .. height - 1]

firstProblem :: CaveMap -> Int
firstProblem caveMap = sum $ map (+ 1) lowPointValues
  where
    lowPointValues = map (strictLookup caveMap) $ getLowPoints caveMap

secondProblem :: CaveMap -> Int
secondProblem caveMap = product $ take 3 $ reverse $ sort basinSizes
  where
    basinSizes = map (length . getBasin caveMap) $ getLowPoints caveMap