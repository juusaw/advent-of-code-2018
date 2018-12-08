import Data.Char
import Data.List
import Data.Ord
import qualified Data.Map as M

opposites :: Char -> Char -> Bool
opposites a b 
  | a == b = False
  | a == toUpper b = True
  | b == toUpper a = True
  | otherwise = False

destroy :: String -> String -> String
destroy [] (a:b:ys)
  | opposites a b = destroy [] ys
  | otherwise = destroy [a] (b:ys)
destroy (a:xs) (b:c:ys)
  | opposites a b = destroy (xs) (c:ys)
  | otherwise = (destroy (b:a:xs) (c:ys))
destroy xs (a:[]) = a:xs
destroy xs [] = xs

charPred :: Char -> Char -> Bool
charPred a b
  | a == b = False
  | toUpper a == b = False
  | otherwise = True

removePolymer :: Char -> String -> String
removePolymer c s = filter (charPred c) s

main = do
  polymers <- readFile("input.txt")
  let destroyed = destroy [] polymers
  print $ length destroyed
  let lenMap = foldl (\m c -> M.insert c (length $ destroy [] $ removePolymer c destroyed) m) M.empty ['a'..'z']
  print $ snd . head . sortBy (comparing snd) $ M.toList lenMap
