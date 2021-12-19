{-# LANGUAGE TupleSections #-}

import Data.Char (isNumber)
import Data.List (sort)

main = do
  testArea <- readInput "day17/test_input"
  area <- readInput "day17/input"
  print $ "Test input: " ++ show (firstProblem testArea) ++ " == 45"
  print $ "Problem input: " ++ show (firstProblem area) ++ " == 9180"
  print $ "Test input: " ++ show (secondProblem testArea) ++ " == 112"
  print $ "Problem input: " ++ show (secondProblem area) ++ " == 3767"
  where
    readInput file = parseArea <$> readFile file

type Position = (Int, Int)

type Velocities = (Int, Int)

data Area = Area {x :: (Int, Int), y :: (Int, Int)}

-- Format: target area: x=20..30, y=-10..-5
parseArea :: String -> Area
parseArea s = buildArea $ map parsePair $ drop 2 $ words s
  where
    buildArea ([x1, x2] : [y1, y2] : _) = Area (x1, x2) (y1, y2)
    buildArea _ = error $ "Invalid input: " ++ s
    parsePair (_ : _ : s) = sort [parseFirst s, parseSecond s]
    parsePair _ = error $ "Invalid input: " ++ s
    parseFirst = read . takeWhile (/= '.')
    parseSecond = read . filter (`notElem` ".,") . dropWhile (/= '.')

maxYVelocity :: Area -> Int
maxYVelocity area
  | positiveY = snd $ y area
  | otherwise = (+ (-1)) $ abs $ fst $ y area
  where
    -- If 0 is in the area there won't be a solution, it wil be infinite
    positiveY = fst (y area) > 0

minYVelocity :: Area -> Int
minYVelocity area = fst $ y area

maxXVelocity :: Area -> Int
maxXVelocity area
  | maxX > 0 = maxX
  | otherwise = maximum $ map (\a -> sum [a .. 0]) [maxX .. 0]
  where
    maxX = snd (x area)

minXVelocity :: Area -> Int
minXVelocity area
  | minX > 0 = minimum $ map (\a -> sum [0 .. a]) [0 .. minX]
  | otherwise = minX
  where
    minX = fst (x area)

validLaunch :: Area -> Velocities -> Bool
validLaunch a velocities = validLaunchFromPos a velocities (0, 0)

validLaunchFromPos :: Area -> Velocities -> Position -> Bool
validLaunchFromPos a@(Area xLimits yLimits) vel pos
  | isInArea a pos = True
  | newYPos < fst yLimits = False
  | otherwise = validLaunchFromPos a (newXVel, newYVel) (newXPos, newYPos)
  where
    newYVel = snd vel - 1
    newXVel = getNewXVel $ fst vel
    getNewXVel old
      | old == 0 = 0
      | old > 0 = old - 1
      | otherwise = old + 1
    newXPos = fst pos + fst vel
    newYPos = snd pos + snd vel

isInArea :: Area -> Position -> Bool
isInArea (Area (x1, x2) (y1, y2)) (x, y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

firstProblem :: Area -> Int
firstProblem area = sum [1 .. maxYVelocity area]

secondProblem :: Area -> Int
secondProblem area = length $ filter (validLaunch area) possibleVelocities
  where
    possibleVelocities =
      concatMap (\a -> map (a,) possibleYVelocities) possibleXVelocities
    possibleYVelocities = [minYVelocity area .. maxYVelocity area]
    possibleXVelocities = [minXVelocity area .. maxXVelocity area]