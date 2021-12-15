import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map (empty, insert)

main :: IO ()
main = do
  (testTemplate, testRules) <- readInput "day14/test_input"
  (template, rules) <- readInput "day14/input"
  print $ "Test input: " ++ show (firstProblem testTemplate testRules) ++ " == 1558"
  print $ "Problem input: " ++ show (firstProblem template rules) ++ " == *"
  where
    readInput file = parseInput . splitOn [""] . lines <$> readFile file
    parseInput (template : rules : _) = (head template, ruleFromStrings rules)
    parseInput _ = error "Invalid input"

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

firstProblem :: String -> Rules -> Int
firstProblem polymer rules = max - min
  where
    max = maximum counts
    min = minimum counts
    counts = map length $ group $ sort after5Steps
    after5Steps = iterate (`applyRules` rules) polymer !! 10

applyRules :: String -> Rules -> String
applyRules polymer rules =
  (head polymer :) $ foldr foldIntoNewPolymer "" (zip polymer $ tail polymer)
  where
    foldIntoNewPolymer key new = (rules ! key) : snd key : new