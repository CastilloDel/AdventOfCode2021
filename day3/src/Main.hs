import Data.Char (digitToInt)
import Data.List (group, maximumBy, sort, transpose)

main :: IO ()
main = do
  lines <- lines <$> readFile "day3/input"
  print $ firstProblem lines

firstProblem :: [String] -> Int
firstProblem lines = binaryToDecimal gamma * binaryToDecimal epsilon
  where
    gamma = getGamma lines
    epsilon = getEpsilonFromGamma gamma

getGamma :: [String] -> String
getGamma = foldr (\a acc -> mostFrequent a : acc) "" . transpose

mostFrequent :: String -> Char
mostFrequent = head . maximumBy (\a b -> length a `compare` length b) . group . sort

getEpsilonFromGamma :: String -> String
getEpsilonFromGamma = map (\a -> if a == '0' then '1' else '0')

binaryToDecimal :: String -> Int
binaryToDecimal = foldl (\acc digit -> acc * 2 + digitToInt digit) 0