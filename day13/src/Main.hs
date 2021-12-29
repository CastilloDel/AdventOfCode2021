import Data.List.Split (splitOn)
import Data.Matrix (Matrix, prettyMatrix)
import qualified Data.Matrix as Matrix (fromList, setElem)
import Data.Set (Set, empty)
import qualified Data.Set as Set (fromList, map, size, toList)

main :: IO ()
main = do
  (testPoints, testInstructions) <- readInput "day13/test_input"
  (points, instructions) <- readInput "day13/input"
  print $ "Test input: " ++ show (firstProblem testPoints testInstructions) ++ " == 17"
  print $ "Problem input: " ++ show (firstProblem points instructions) ++ " == 701"
  print "Test input:"
  print $ secondProblem testPoints testInstructions
  print "Problem input:"
  print $ secondProblem points instructions
  where
    readInput file = parseInput . splitOn [""] . lines <$> readFile file
    parseInput (points : instructions : _) =
      ( Set.fromList $ map pointFromString points,
        map instructionFromString instructions
      )
    parseInput _ = error "Invalid input"

type Point = (Int, Int)

pointFromString :: String -> Point
pointFromString s = fromParts $ splitOn "," s
  where
    fromParts (x : y : _) = (read x, read y)
    fromParts _ = error $ "Invalid point: " ++ s

type Points = Set Point

data Instruction = X Int | Y Int deriving (Show, Eq)

instructionFromString :: String -> Instruction
instructionFromString s = fromParts $ splitOn "=" $ words s !! 2
  where
    fromParts ("x" : val : _) = X (read val)
    fromParts ("y" : val : _) = Y (read val)
    fromParts _ = error $ "Invalid instruction: " ++ s

applyInstruction :: Point -> Instruction -> Point
applyInstruction (x, y) (X val) = (foldAround val x, y)
applyInstruction (x, y) (Y val) = (x, foldAround val y)

foldAround :: Int -> Int -> Int
foldAround center val = if val < center then val else center * 2 - val

firstProblem :: Points -> [Instruction] -> Int
firstProblem points instructions =
  Set.size $ Set.map (`applyInstruction` instruction) points
  where
    instruction = head instructions

secondProblem :: Points -> [Instruction] -> Matrix Char
secondProblem points = matrixFromPoints . foldl mapInstruction points
  where
    mapInstruction set instruction = Set.map (`applyInstruction` instruction) set

matrixFromPoints :: Points -> Matrix Char
matrixFromPoints points = foldl foldMatrix emptyMatrix points
  where
    emptyMatrix = Matrix.fromList rows cols $ replicate (rows * cols) ' '
    rows = maximum $ map ((+ 1) . snd) $ Set.toList points
    cols = maximum $ map ((+ 1) . fst) $ Set.toList points
    -- Matrix indexes start at 1
    foldMatrix m (j, i) = Matrix.setElem 'x' (i + 1, j + 1) m