{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first)
import Data.List (transpose)
import Data.Map (Map, (!))
import qualified Data.Map as Map (empty, insert, member)
import Data.Maybe (catMaybes, isJust, isNothing, mapMaybe)

main :: IO ()
main = do
  testRooms <- readInput "day23/test_input"
  rooms <- readInput "day23/input"
  print $ "Test input: " ++ show (firstProblem testRooms) ++ " == 12521"
  print $ "Problem input: " ++ show (firstProblem rooms) ++ " == 15109"
  where
    readInput file = parseInput . take 2 . drop 2 . lines <$> readFile file
    parseInput = transpose . map (map (,False) . mapMaybe parseAmphipod)

data Amphipod = A | B | C | D deriving (Show, Eq, Ord)

parseAmphipod :: Char -> Maybe Amphipod
parseAmphipod 'A' = Just A
parseAmphipod 'B' = Just B
parseAmphipod 'C' = Just C
parseAmphipod 'D' = Just D
parseAmphipod _ = Nothing

getMoveCost :: Amphipod -> Int
getMoveCost A = 1
getMoveCost B = 10
getMoveCost C = 100
getMoveCost D = 1000

getRoomIndex :: Amphipod -> Int
getRoomIndex A = 0
getRoomIndex B = 1
getRoomIndex C = 2
getRoomIndex D = 3

type State = ([Room], Hallway)

-- The bool tells whether the amphipod has already moved or not
type Room = [(Amphipod, Bool)]

type Hallway = [Maybe Amphipod]

type Mem = Map State Int

updateMem :: State -> Maybe Int -> Mem -> Mem
updateMem state (Just val) mem = Map.insert state val mem
updateMem state Nothing mem = mem

hallwaySize = 7

firstProblem :: [Room] -> Maybe Int
firstProblem rooms =
  fst $ exploreSolutions Map.empty roomSize (rooms, hallway)
  where
    hallway = replicate hallwaySize Nothing
    roomSize = length $ head rooms

exploreSolutions :: Mem -> Int -> State -> (Maybe Int, Mem)
exploreSolutions mem size state =
  if Map.member state mem
    then (Just $ mem ! state, mem)
    else (bestPartialSolution, updateMem state bestPartialSolution newMem)
  where
    bestPartialSolution = if isEnd state then Just 0 else bestRecursiveSolution
    bestRecursiveSolution =
      if null solutions then Nothing else Just $ minimum solutions
    (solutions, newMem) = foldl (foldIntoMem size) ([], mem) newStates
    newStates = getPossibleSteps size state

foldIntoMem :: Int -> ([Int], Mem) -> (State, Int) -> ([Int], Mem)
foldIntoMem size (list, mem) (newState, weight) = (newList, newMem)
  where
    (solution, newMem) = exploreWithWeight mem size newState weight
    newList = maybe list (: list) solution

exploreWithWeight :: Mem -> Int -> State -> Int -> (Maybe Int, Mem)
exploreWithWeight mem size state weight =
  first (fmap (+ weight)) $ exploreSolutions mem size state

isEnd :: State -> Bool
isEnd ([a, b, c, d], hallway) =
  isRoomFilledWith A a
    && isRoomFilledWith B b
    && isRoomFilledWith C c
    && isRoomFilledWith D d
    && all isNothing hallway
isEnd _ = error "Invalid number of rooms"

isRoomFilledWith :: Amphipod -> Room -> Bool
isRoomFilledWith amp = all ((== amp) . fst)

getPossibleSteps :: Int -> State -> [(State, Int)]
getPossibleSteps size state =
  if null moveIntoRoom
    then getRoomsSteps size state
    else moveIntoRoom
  where
    -- We prioritize these movements, because they are always correct if they can be done
    moveIntoRoom = getHallwaySteps size state

getHallwaySteps :: Int -> State -> [(State, Int)]
getHallwaySteps size state@(_, hallway) =
  mapMaybe (getHallwayPosStep size state) $ zip hallway [0 .. 6]

getHallwayPosStep :: Int -> State -> (Maybe Amphipod, Int) -> Maybe (State, Int)
getHallwayPosStep size (rooms, hallway) (Just amp, pos)
  | not (isRoomFull room)
      && isRoomFilledWith amp room
      && isPathFree hallway roomIndex pos =
    Just ((newRooms, newHallway), cost)
  where
    newRooms = changeItem rooms roomIndex newRoom
    newRoom = (amp, True) : room
    newHallway = changeItem hallway pos Nothing
    cost = getMoveCost amp * getDistance size room roomIndex pos
    room = rooms !! roomIndex
    roomIndex = getRoomIndex amp
getHallwayPosStep _ _ _ = Nothing

getRoomsSteps :: Int -> State -> [(State, Int)]
getRoomsSteps size state = concatMap (getRoomSteps size state) [0 .. 3]

getRoomSteps :: Int -> State -> Int -> [(State, Int)]
getRoomSteps size state@(rooms, hallway) roomIndex =
  if null room || snd (head room) -- If the top amphipod has already moved it can't do it again
    then []
    else mapMaybe (getRoomPosStep size state roomIndex) [0 .. length hallway - 1]
  where
    room = rooms !! roomIndex

getRoomPosStep :: Int -> State -> Int -> Int -> Maybe (State, Int)
getRoomPosStep size (rooms, hallway) roomIndex pos
  | isNothing (hallway !! pos)
      && isPathFree hallway roomIndex pos =
    Just ((newRooms, newHallway), cost)
  where
    newRooms = changeItem rooms roomIndex newRoom
    newRoom = tail room
    newHallway = changeItem hallway pos (Just amp)
    cost = getMoveCost amp * getDistance size newRoom roomIndex pos
    amp = fst $ head room
    room = rooms !! roomIndex
getRoomPosStep _ _ _ _ = Nothing

isRoomFull :: Room -> Bool
isRoomFull = (== 4) . length

isPathFree :: Hallway -> Int -> Int -> Bool
isPathFree hallway roomIndex pos
  | pos == lower || pos == upper = True
  | pos > upper = all isNothing $ take (pos - upper) $ drop upper hallway
  | otherwise = all isNothing $ take (lower - pos) $ drop (pos + 1) hallway
  where
    lower = roomIndex + 1
    upper = roomIndex + 2

getDistance :: Int -> Room -> Int -> Int -> Int
getDistance size room roomIndex pos = hallwayDistance + roomDistance
  where
    -- Adjust to account for the fact we don't store the room entries
    hallwayDistance = 2 * hallwayDistance' + (if isLimit then 0 else 1)
    isLimit = pos == 0 || pos == hallwaySize
    hallwayDistance' = min (abs $ roomIndex + 1 - pos) (abs $ roomIndex + 2 - pos)
    roomDistance = size - length room

changeItem :: [a] -> Int -> a -> [a]
changeItem list index item = take index list ++ item : drop (index + 1) list