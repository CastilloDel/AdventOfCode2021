import Control.Monad.State (State, evalState, gets, modify, replicateM)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Ix (range)
import Data.Matrix (Matrix, fromLists, ncols, nrows, setElem, (!))

main :: IO ()
main = do
  testInput <- readInput "day11/test_input"
  input <- readInput "day11/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 1656"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 1655"
  print $ "Test input: " ++ show (secondProblem testInput) ++ " == 195"
  print $ "Problem input: " ++ show (secondProblem input) ++ " == 337"
  where
    readInput file = fromLists . map (map digitToInt) . lines <$> readFile file

type Octopus = Int

isFlasher :: Octopus -> Bool
isFlasher val
  | val > 9 = True
  | otherwise = False

flash :: Octopus -> Octopus
flash = const 0

applyFlash :: Octopus -> Octopus
applyFlash 0 = 0
applyFlash octopus = octopus + 1

type OctopusMap = Matrix Octopus

findFlashers :: OctopusMap -> [Position]
findFlashers matrix = foldl foldIfFlasher [] indexes
  where
    foldIfFlasher acc pos =
      if isFlasher $ matrix ! pos
        then pos : acc
        else acc
    -- Matrix indexes start at 1
    indexes = range ((1, 1), (nrows matrix, ncols matrix))

updateOctopuses :: (Octopus -> Octopus) -> [Position] -> OctopusMap -> OctopusMap
updateOctopuses f list m = foldl folder m list
  where
    folder matrix pos = setElem (f $ matrix ! pos) pos matrix

type Position = (Int, Int)

getNeighbours :: Position -> [Position]
getNeighbours (i, j) =
  map (Data.Bifunctor.bimap (i +) (j +)) $ range ((-1, -1), (1, 1))

isValidPos :: Matrix a -> Position -> Bool
-- Matrix indexes start at 1
isValidPos m (i, j) = i > 0 && j > 0 && i <= nrows m && j <= ncols m

step :: State OctopusMap Int
step =
  do
    modify $ fmap (+ 1)
    flashOctopuses

flashOctopuses :: State OctopusMap Int
flashOctopuses = do
  flashers <- gets findFlashers
  if null flashers
    then return 0
    else do
      modify $ updateOctopuses flash flashers
      let possibleFlashed = concatMap getNeighbours flashers
      flashed <- gets (\m -> filter (isValidPos m) possibleFlashed)
      modify $ updateOctopuses applyFlash flashed
      newFlashersNumber <- flashOctopuses
      return $ length flashers + newFlashersNumber

firstProblem :: OctopusMap -> Int
firstProblem = sum . evalState (replicateM 100 step)

secondProblem :: OctopusMap -> Int
secondProblem m = evalState (countStepsUntil (== nrows m * ncols m)) m

countStepsUntil :: (Int -> Bool) -> State OctopusMap Int
countStepsUntil p = try 1
  where
    try n = do
      newFlashersNumber <- step
      if p newFlashersNumber then return n else try (n + 1)