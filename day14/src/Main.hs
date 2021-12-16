import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map (empty, filterWithKey, foldlWithKey, insert, insertWith, map, toList)
import Debug.Trace (trace)

main :: IO ()
main = do
  (testTemplate, testRules) <- readInput "day14/test_input"
  (template, rules) <- readInput "day14/input"
  print $ "Test input: " ++ show (firstProblem testTemplate testRules) ++ " == 1558"
  print $ "Problem input: " ++ show (firstProblem template rules) ++ " == 3048"
  print $ "Test input: " ++ show (secondProblem testTemplate testRules) ++ " == 2188189693529"
  print $ "Problem input: " ++ show (secondProblem template rules) ++ " == 3288891573057"
  where
    readInput file = parseInput . splitOn [""] . lines <$> readFile file
    parseInput (template : rules : _) =
      (polymerFromString $ head template, ruleFromStrings rules)
    parseInput _ = error "Invalid input"

type Polymer = Map (Char, Char) Int

-- Each element is paired twice, with the element on its left and its right.
-- Converting to Polymer discards the order of the elements,
-- but it is okay, because we don't need them.
polymerFromString :: String -> Polymer
polymerFromString s = foldl foldIntoPolymer Map.empty zippedS
  where
    zippedS = zip sWithLimits $ tail sWithLimits
    -- We add spaces to know which letters are in the borders
    sWithLimits = ' ' : s ++ [' ']
    foldIntoPolymer map key = Map.insertWith (+) key 1 map

type Rules = Map (Char, Char) Char

ruleFromStrings :: [String] -> Rules
ruleFromStrings = foldl foldIntoMap Map.empty . map ruleKeyValFromStrings
  where
    foldIntoMap map (key, val) = Map.insert key val map

ruleKeyValFromStrings :: String -> ((Char, Char), Char)
ruleKeyValFromStrings s = fromParts $ splitOn " -> " s
  where
    fromParts (pair : insertion : _) = ((head pair, pair !! 1), head insertion)
    fromParts _ = error $ "Invalid input: " ++ s

firstProblem :: Polymer -> Rules -> Int
firstProblem = problem 10

secondProblem :: Polymer -> Rules -> Int
secondProblem = problem 40

problem :: Int -> Polymer -> Rules -> Int
problem steps polymer rules = max - min
  where
    max = maximum counts
    min = minimum counts
    -- We need to divide by two because each value is counted twice, left and right
    -- and we also need to remove the spaces
    counts = Map.map (`div` 2) $ Map.filterWithKey (\k _ -> k /= ' ') countsMap
    countsMap = Map.foldlWithKey foldIntoCount Map.empty afterNSteps
    foldIntoCount counts key val =
      Map.insertWith (+) (snd key) val $
        Map.insertWith (+) (fst key) val counts
    afterNSteps = iterate (`applyRules` rules) polymer !! steps

applyRules :: Polymer -> Rules -> Polymer
applyRules polymer rules =
  Map.foldlWithKey foldIntoNewPolymer Map.empty polymer
  where
    foldIntoNewPolymer new key val
      -- Skip the borders
      | fst key == ' ' || snd key == ' ' = Map.insert key val new
      | otherwise =
        let insertion = rules ! key
         in Map.insertWith (+) (insertion, snd key) val $
              Map.insertWith (+) (fst key, insertion) val new