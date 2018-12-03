import System.IO
import Data.List

isSimilar :: Eq a => [a] -> [a] -> Bool -> Bool
isSimilar [] [] _ = True
isSimilar [] __ _ = error ""
isSimilar __ [] _ = error ""
isSimilar (x:xs) (y:ys) True
  | x == y = isSimilar xs ys True
  | otherwise = False
isSimilar (x:xs) (y:ys) False = isSimilar xs ys (x /= y)

findMatchingPair :: [(String, String)] -> (String, String)
findMatchingPair [] = error ""
findMatchingPair ((a, b):rest)
  | isSimilar a b False = (a, b)
  | otherwise = findMatchingPair rest

createPairs :: [String] -> [(String, String)] -> [(String, String)]
createPairs [] pairs = pairs
createPairs (x:xs) pairs = createPairs xs (pairs ++ map (\el -> (x, el)) xs)

main = do
  content <- readFile("input.txt")
  let l = lines content
  let pairs = createPairs l []
  let (a, b) = findMatchingPair pairs
  print $ map fst $ filter (\(x, y) -> x == y) $ zip a b
