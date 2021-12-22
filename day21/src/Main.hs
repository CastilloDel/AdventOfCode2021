{-# LANGUAGE TupleSections #-}

import qualified Data.Bifunctor as Bifunctor

main :: IO ()
main = do
  testStartingPositions <- readInput "day21/test_input"
  startingPositions <- readInput "day21/input"
  print $ "Test input: " ++ show (firstProblem testStartingPositions) ++ " == 739785"
  print $ "Problem input: " ++ show (firstProblem startingPositions) ++ " == *"
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