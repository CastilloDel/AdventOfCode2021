import Data.Char (isNumber)

main = do
  testArea <- readInput "day17/test_input"
  area <- readInput "day17/input"
  print $ "Test input: " ++ show (firstProblem testArea) ++ " == 45"
  print $ "Problem input: " ++ show (firstProblem area) ++ " == 9180"
  where
    readInput file = parseArea <$> readFile file

data Area = Area {x :: (Int, Int), y :: (Int, Int)}

-- Format: target area: x=20..30, y=-10..-5
parseArea :: String -> Area
parseArea s = buildArea $ map parsePair $ drop 2 $ words s
  where
    buildArea (x : y : _) = Area x y
    buildArea _ = error $ "Invalid input: " ++ s
    parsePair (_ : _ : s) = (parseFirst s, parseSecond s)
    parsePair _ = error $ "Invalid input: " ++ s
    parseFirst = read . takeWhile (/= '.')
    parseSecond = read . filter (`notElem` ".,") . dropWhile (/= '.')

firstProblem :: Area -> Int
firstProblem area = sum [1 .. maxHeight]
  where
    maxHeight =
      if positiveY
        then uncurry max $ y area
        else (+ (-1)) $ abs $ uncurry min $ y area
    -- If 0 is in the area there won't be a solution, it wil be infinite
    positiveY = fst (y area) > 0