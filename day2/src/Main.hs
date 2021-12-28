import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  testInstructions <- mapMaybe parseInstruction . lines <$> readFile "day2/test_input"
  instructions <- mapMaybe parseInstruction . lines <$> readFile "day2/input"
  print $ "Test input: " ++ show (firstProblem testInstructions) ++ " == 150"
  print $ "Problem input: " ++ show (firstProblem instructions) ++ " == 1383564"
  print $ "Test input: " ++ show (secondProblem testInstructions) ++ " == 900"
  print $ "Problem input: " ++ show (secondProblem instructions) ++ " == 1488311643"
  where
    firstProblem = problem applyInstruction1
    secondProblem = problem applyInstruction2

data Instruction = Forward Int | Down Int | Up Int

parseInstruction :: String -> Maybe Instruction
parseInstruction input
  | word == "forward" = createInstruction Forward
  | word == "down" = createInstruction Down
  | word == "up" = createInstruction Up
  | otherwise = Nothing
  where
    createInstruction instructionType = Just $ instructionType number
    word = head contents
    number = read $ last contents
    contents = words input

data Submarine = Submarine {position :: Int, depth :: Int, aim :: Int}

applyInstruction1 :: Submarine -> Instruction -> Submarine
applyInstruction1 s@Submarine {position = p} (Forward val) = s {position = p + val}
applyInstruction1 s@Submarine {depth = d} (Down val) = s {depth = d + val}
applyInstruction1 s@Submarine {depth = d} (Up val) = s {depth = d - val}

applyInstruction2 :: Submarine -> Instruction -> Submarine
applyInstruction2 Submarine {position = p, depth = d, aim = a} (Forward val) =
  Submarine {position = p + val, depth = d + a * val, aim = a}
applyInstruction2 s@Submarine {aim = a} (Down val) = s {aim = a + val}
applyInstruction2 s@Submarine {aim = a} (Up val) = s {aim = a - val}

-- We apply the instructions with the provided function and then calculate
-- the product of depth and position
problem :: (Submarine -> Instruction -> Submarine) -> [Instruction] -> Int
problem apply instructions =
  multiplySubmarineData $ foldl apply initial instructions
  where
    initial = Submarine {position = 0, depth = 0, aim = 0}
    multiplySubmarineData = \Submarine {depth = d, position = p} -> d * p