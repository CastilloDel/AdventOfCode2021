import Control.Arrow (Arrow (first))
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, map, size)

main :: IO ()
main = do
  (testPoints, testInstructions) <- readInput "day13/test_input"
  (points, instructions) <- readInput "day13/input"
  print $ "Test input: " ++ show (firstProblem testPoints testInstructions) ++ " == 17"
  print $ "Problem input: " ++ show (firstProblem points instructions) ++ " == *"
  where
    readInput file = parseInput . splitOn [""] . lines <$> readFile file
    parseInput (points : instructions : _) =
      ( Set.fromList $ map pointFromString points,
        map instructionFromString instructions
      )
    parseInput _ = error "Invalid input"

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)

pointFromString :: String -> Point
pointFromString s = fromParts $ splitOn "," s
  where
    fromParts (x : y : _) = Point (read x) (read y)
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
applyInstruction (Point x y) (X val) = Point (foldAround val x) y
applyInstruction (Point x y) (Y val) = Point x (foldAround val y)

foldAround :: Int -> Int -> Int
foldAround center val = if val < center then val else center * 2 - val

firstProblem :: Points -> [Instruction] -> Int
firstProblem points instructions =
  Set.size $ Set.map (`applyInstruction` instruction) points
  where
    instruction = head instructions