{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (digitToInt)
import Data.Ix (range)
import Data.List (sort, (\\))
import Data.Map (Map)
import qualified Data.Map as Map (fromList, (!))

main :: IO ()
main = do
  testInput <- readInput "day9/test_input"
  input <- readInput "day9/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 15"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 496"
  print $ "Test input: " ++ show (secondProblem testInput) ++ " == 1134"
  print $ "Problem input: " ++ show (secondProblem input) ++ " == 902880"
  where
    readInput = fmap (caveMapFromMatrix . map (map digitToInt) . lines) . readFile

data CaveMap = CaveMap {height :: Int, width :: Int, contents :: Map (Int, Int) Int}

caveMapFromMatrix :: [[Int]] -> CaveMap
caveMapFromMatrix matrix = CaveMap {height, width, contents}
  where
    height = length matrix
    width = length $ head matrix
    contents = Map.fromList $ zip indexes $ concat matrix
    indexes = indexesFromSize height width

(!) :: CaveMap -> (Int, Int) -> Int
(!) CaveMap {contents = contents} key = contents Map.! key

isValidPoint :: CaveMap -> (Int, Int) -> Bool
isValidPoint CaveMap {height, width} (i, j) =
  not $ i < 0 || j < 0 || i >= height || j >= width

isLowPoint :: CaveMap -> (Int, Int) -> Bool
isLowPoint caveMap@CaveMap {height, width} index = all (> value) adjacentValues
  where
    value = caveMap ! index
    adjacentValues = map (caveMap !) adjacentIndexes
    adjacentIndexes = filter (isValidPoint caveMap) $ getAdjacentPositions index

getLowPoints :: CaveMap -> [(Int, Int)]
getLowPoints caveMap@CaveMap {height, width} =
  filter (isLowPoint caveMap) $ indexesFromSize height width

getBasin :: CaveMap -> (Int, Int) -> [(Int, Int)]
getBasin caveMap = getBasinIncomplete caveMap []

getBasinIncomplete :: CaveMap -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
getBasinIncomplete caveMap explored new =
  foldl (getBasinIncomplete caveMap) newExplored newPoints
  where
    newExplored = explored ++ newPoints
    newPoints = pointsInBasin \\ explored
    pointsInBasin = filter ((< 9) . (caveMap !)) possibleBasinPoints
    possibleBasinPoints = filter (isValidPoint caveMap) $ getAdjacentPositions new

getAdjacentPositions :: (Int, Int) -> [(Int, Int)]
getAdjacentPositions (i, j) = [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]

indexesFromSize :: Int -> Int -> [(Int, Int)]
indexesFromSize height width = range ((0, 0), (height - 1, width - 1))

firstProblem :: CaveMap -> Int
firstProblem caveMap = sum $ map (+ 1) lowPointValues
  where
    lowPointValues = map (caveMap !) $ getLowPoints caveMap

secondProblem :: CaveMap -> Int
secondProblem caveMap = product $ take 3 $ reverse $ sort basinSizes
  where
    basinSizes = map (length . getBasin caveMap) $ getLowPoints caveMap