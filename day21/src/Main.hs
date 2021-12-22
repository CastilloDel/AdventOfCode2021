{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first, second)
import qualified Data.Bifunctor as Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, member, (!))

main :: IO ()
main = do
  testStartingPositions <- readInput "day21/test_input"
  startingPositions <- readInput "day21/input"
  print $ "Test input: " ++ show (firstProblem testStartingPositions) ++ " == 739785"
  print $ "Problem input: " ++ show (firstProblem startingPositions) ++ " == 906093"
  print $ "Test input: " ++ show (secondProblem testStartingPositions) ++ " == 444356092776315"
  print $ "Problem input: " ++ show (secondProblem startingPositions) ++ " == 274291038026362"
  where
    readInput file = map parseInput . lines <$> readFile file
    parseInput line = read (drop (length line - 2) line) :: Int

type Player = (Position, Points)

type Position = Int

type Points = Int

type Die = Int

firstProblem :: [Position] -> Int
firstProblem positions = calculateResult $ playGame players 1 0
  where
    players = map (,0) positions
    calculateResult (players, rolls) = rolls * minimum (map snd players)

playGame :: [Player] -> Die -> Int -> ([Player], Int)
playGame ((pos, points) : rest) die rolls =
  if newPoints >= 1000
    then ((newPos, newPoints) : rest, nextRolls)
    else playGame (rest ++ [(newPos, newPoints)]) nextDie nextRolls
  where
    newPos = addWithWrap 10 (getRoll die) pos
    newPoints = points + newPos
    nextDie = getNextDie die
    nextRolls = rolls + 3
playGame _ _ _ = error "Empty player list"

getNextDie :: Die -> Die
getNextDie = addWithWrap 100 3

getRoll :: Die -> Int
getRoll die = sum $ take 3 $ iterate (addWithWrap 100 1) die

addWithWrap :: Int -> Int -> Int -> Int
addWithWrap top toAdd val = ((val + toAdd - 1) `mod` top) + 1

type Mem = Map (Player, Player) Score

type Score = (Int, Int) -- In how many universes a player has won

secondProblem :: [Position] -> Int
secondProblem positions = uncurry max $ fst $ playQuantumGame Map.empty players
  where
    players = playersToTuple $ take 2 $ map (,0) positions
    playersToTuple a = (head a, last a)

playQuantumGame :: Mem -> (Player, Player) -> (Score, Mem)
playQuantumGame mem key@(p1, p2)
  | Map.member key mem = (mem Map.! key, mem)
  | otherwise =
    foldl foldIntoResult ((0, 0), mem) possibleNewPlayers
  where
    foldIntoResult (score, mem) (newP1, weight) =
      updateMem $ first (updateScore score weight) $ handlePlayer mem newP1 p2
    updateScore actual weight = sumTuple actual . mulTuple weight
    updateMem (score, mem) = (score, Map.insert key score mem)
    possibleNewPlayers = map (first $ getNewPlayer p1) posibilities
    posibilities = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

getNewPlayer :: Player -> Int -> Player
getNewPlayer (pos, points) n = (newPos, newPos + points)
  where
    newPos = addWithWrap 10 n pos

handlePlayer :: Mem -> Player -> Player -> (Score, Mem)
handlePlayer mem player@(_, points) p2 =
  if points >= 21
    then ((1, 0), mem)
    else first (\(a, b) -> (b, a)) $ playQuantumGame mem (p2, player)

sumTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuple (a, b) (c, d) = (a + c, b + d)

mulTuple :: Int -> (Int, Int) -> (Int, Int)
mulTuple n (a, b) = (a * n, b * n)