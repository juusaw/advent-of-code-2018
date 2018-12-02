import System.IO
import Data.List

extractNum :: Int -> String -> Bool
extractNum num str = elem num $ map length $ group $ sort str

countTrues :: [Bool] -> Int
countTrues arr = length $ filter id $ arr

main = do
  content <- readFile("input.txt")
  let l = lines content
  let twos = countTrues $ map (extractNum 2) l
  let threes = countTrues $ map (extractNum 3) l
  print $ twos * threes
