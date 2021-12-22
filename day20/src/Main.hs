import Data.Array (Array, listArray, range)
import qualified Data.Array as Array ((!))
import qualified Data.Bifunctor as Bifunctor
import Data.List (groupBy)
import Data.Matrix (Matrix, ncols, nrows)
import qualified Data.Matrix as Matrix (fromLists, toList, (!))

main :: IO ()
main = do
  (testAlgorithm, testImage) <- readInput "day20/test_input"
  (algorithm, image) <- readInput "day20/input"
  print $ "Test input: " ++ show (firstProblem testAlgorithm testImage) ++ " == 35"
  print $ "Problem input: " ++ show (firstProblem algorithm image) ++ " == 5461"
  print $ "Test input: " ++ show (secondProblem testAlgorithm testImage) ++ " == 3351"
  print $ "Problem input: " ++ show (secondProblem algorithm image) ++ " == 18226"
  where
    readInput file = parseInput . map (map (== '#')) . lines <$> readFile file
    parseInput (algorithm : _ : image) = (parseAlgorithm algorithm, parseImage image)
    parseInput _ = error "Parse error"
    parseAlgorithm algorithm = listArray (0, length algorithm) algorithm
    parseImage image = Matrix.fromLists image

type Algorithm = Array Int Bool

type Image = Matrix Bool

type Position = (Int, Int)

firstProblem :: Algorithm -> Image -> Int
firstProblem = problem 2

secondProblem :: Algorithm -> Image -> Int
secondProblem = problem 50

problem :: Int -> Algorithm -> Image -> Int
problem n algorithm image =
  length $ filter id $ Matrix.toList $ fst $ iterate step' (image, False) !! n
  where
    step' (image, def) = step algorithm image def

step :: Algorithm -> Image -> Bool -> (Image, Bool)
step algorithm image def = (Matrix.fromLists newImage, newDefault)
  where
    newDefault = if def then algorithm Array.! 511 else algorithm Array.! 0
    newImage = map (map getNewVal) $ groupBy (\a b -> fst a == fst b) newIndexes
    getNewVal = calculateNewValue algorithm image def
    newIndexes = range ((0, 0), (nrows image + 1, ncols image + 1))

calculateNewValue :: Algorithm -> Image -> Bool -> Position -> Bool
calculateNewValue algorithm image def pos = algorithm Array.! key
  where
    key = binaryToInt values
    values = map (accessOrDefault image def) positions
    positions = getNeighbours pos

accessOrDefault :: Image -> Bool -> Position -> Bool
accessOrDefault image def (i, j) =
  if i >= 1 && j >= 1 && i <= nrows image && j <= ncols image
    then image Matrix.! (i, j)
    else def

getNeighbours :: Position -> [Position]
getNeighbours (i, j) =
  map (Bifunctor.bimap (i +) (j +)) $ range ((-1, -1), (1, 1))

binaryToInt :: [Bool] -> Int
binaryToInt = foldl (\acc bool -> acc * 2 + if bool then 1 else 0) 0
