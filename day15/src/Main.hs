import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map (empty, findWithDefault, insert)
import Data.Matrix (Matrix, ncols, nrows, (!), (<->), (<|>))
import qualified Data.Matrix as Matrix (fromLists)
import Data.Set (Set)
import qualified Data.Set as Set
  ( delete,
    empty,
    fromList,
    insert,
    member,
    notMember,
    toAscList,
  )

main :: IO ()
main = do
  testCavern <- readInput "day15/test_input"
  cavern <- readInput "day15/input"
  print $ "Test input: " ++ show (firstProblem testCavern) ++ " == 40"
  print $ "Problem input: " ++ show (firstProblem cavern) ++ " == 429"
  print $ "Test input: " ++ show (secondProblem testCavern) ++ " == 315"
  print $ "Problem input: " ++ show (secondProblem cavern) ++ " == 2844"
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

secondProblem :: CavernMap -> Int
secondProblem cavern =
  getPathCost (1, 1) (nrows wholeCavern, ncols wholeCavern) wholeCavern
  where
    wholeCavern = foldl1 (<->) $ map getRow [0 .. 4]
    getRow n = foldl1 (<|>) $ map (caverns !!) [n .. n + 4]
    caverns = iterate (fmap addWithOverflow9) cavern
    addWithOverflow9 9 = 1
    addWithOverflow9 val = val + 1

getPathCost :: Point -> Point -> CavernMap -> Int
getPathCost start end cavern = getPathCost' initialPaths Map.empty
  where
    initialPaths = Set.insert (newPath start 0 end) Set.empty
    getPathCost' paths visited =
      if lastPoint == end
        then cost cheapestPath
        else getPathCost' nextPaths nextVisited
      where
        lastPoint = position cheapestPath
        cheapestPath = head $ Set.toAscList paths
        nextPaths = foldr Set.insert (Set.delete cheapestPath paths) newPaths
        nextVisited = foldl foldPointsWithCost visited newPaths
        newPaths = getNewPaths cheapestPath end visited cavern
        foldPointsWithCost m path = Map.insert (position path) (cost path) m

getNewPaths :: Path -> Point -> Map Point Int -> CavernMap -> [Path]
getNewPaths actual end visited cavern =
  filter (\path -> getPathOldCost path > cost path) possibleNewPaths
  where
    getPathOldCost path = Map.findWithDefault maxBound (position path) visited
    possibleNewPaths = map getNewPath $ filter (isValid cavern) neighbours
    neighbours = getNeighbours $ position actual
    getNewPath next = newPath next (cavern ! next + cost actual) end

getNeighbours :: Point -> [Point]
getNeighbours (i, j) = [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]
