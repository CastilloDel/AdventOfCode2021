import Data.Char (digitToInt)
import Data.List (group, maximumBy, sort, transpose)

main :: IO ()
main = do
  lines <- lines <$> readFile "day3/input"
  print $ firstProblem lines
  print $ secondProblem lines

firstProblem :: [String] -> Int
firstProblem lines = binaryToDecimal gamma * binaryToDecimal epsilon
  where
    gamma = getGamma lines
    epsilon = getEpsilonFromGamma gamma

getGamma :: [String] -> String
getGamma = foldr (\val acc -> mostFrequent val : acc) "" . transpose

mostFrequent :: String -> Char
mostFrequent = head . maximumBy (\a b -> length a `compare` length b) . group . sort

getEpsilonFromGamma :: String -> String
getEpsilonFromGamma = map (\val -> if val == '0' then '1' else '0')

secondProblem :: [String] -> Int
secondProblem lines = oxygenRating * co2Rating
  where
    oxygenRating = binaryToDecimal $ getOxygenRating lines
    co2Rating = binaryToDecimal $ getCO2Rating lines

getOxygenRating :: [String] -> String
getOxygenRating = getRating mostFrequent

getCO2Rating :: [String] -> String
getCO2Rating = getRating leastFrequent
  where
    leastFrequent = \val -> if mostFrequent val == '0' then '1' else '0'

getRating :: (String -> Char) -> [String] -> String
getRating _ [x] = x
getRating criteria lines = val : rest
  where
    val = criteria $ head $ transpose lines
    rest = getRating criteria $ map tail $ filter (\line -> head line == val) lines

binaryToDecimal :: String -> Int
binaryToDecimal = foldl (\acc digit -> acc * 2 + digitToInt digit) 0