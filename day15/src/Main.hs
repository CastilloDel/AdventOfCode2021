import Data.Char (digitToInt)
import Data.Matrix (Matrix, ncols, nrows, (!))
import qualified Data.Matrix as Matrix (fromLists)
import Data.Set (Set)
import qualified Data.Set as Set (delete, empty, fromList, insert, member, toAscList)

main :: IO ()
main = do
  testCavern <- readInput "day15/test_input"
  cavern <- readInput "day15/input"
  print $ "Test input: " ++ show (firstProblem testCavern) ++ " == 40"
  print $ "Problem input: " ++ show (firstProblem cavern) ++ " == 429"
  where
    readInput file = fmap digitToInt . Matrix.fromLists . lines <$> readFile file

type CavernMap = Matrix Int

type Point = (Int, Int)

isValid :: CavernMap -> Point -> Bool
isValid cavern (i, j) = i > 0 && j > 0 && i <= nrows cavern && j <= ncols cavern

type Paths = Set Path

data Path = Path
  { position :: Point,
    cost :: Int,
    predictedCost :: Int
  }
  deriving (Eq, Show)

instance Ord Path where
  compare a b
    | a == b = EQ
    | aCost < bCost = LT
    | aCost > bCost = GT
    | otherwise = position a `compare` position b
    where
      aCost = getTotalCost a
      bCost = getTotalCost b

newPath :: Point -> Int -> Point -> Path
newPath pos cost end =
  Path
    { position = pos,
      cost = cost,
      predictedCost = manhattanDistance pos end
    }

manhattanDistance :: Point -> Point -> Int
manhattanDistance (i1, j1) (i2, j2) = abs (i1 - i2) + abs (j1 - j2)

getTotalCost :: Path -> Int
getTotalCost Path {cost = c, predictedCost = pc} = c + pc

firstProblem :: CavernMap -> Int
firstProblem map = getPathCost (1, 1) (nrows map, ncols map) map

getPathCost :: Point -> Point -> CavernMap -> Int
getPathCost start end cavern = getPathCost' initialPaths
  where
    initialPaths = Set.insert (newPath start 0 end) Set.empty
    getPathCost' paths =
      let cheapestPath = head $ Set.toAscList paths
          lastPoint = position cheapestPath
          paths' = Set.delete cheapestPath paths
          newPaths = getNewPaths cheapestPath end cavern
          nextPaths = foldr Set.insert paths' newPaths
       in if lastPoint == end
            then cost cheapestPath
            else getPathCost' nextPaths

getNewPaths :: Path -> Point -> CavernMap -> [Path]
getNewPaths actual end cavern =
  map getNewPath $ filter (isValid cavern) $ getNeighbours $ position actual
  where
    getNewPath next = newPath next (cavern ! next + cost actual) end

getNeighbours :: Point -> [Point]
getNeighbours (i, j) = [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]
