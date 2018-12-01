import System.IO
import Data.Maybe

readInt :: String -> Int
readInt ('+':rest) = read rest
readInt all = read all

findDupl :: Show a => Num a => Eq a => [a] -> [a] -> Maybe a
findDupl a [] = Nothing
findDupl [] (x:xs) = findDupl [x] xs
findDupl (x:xs) (head:tail)
  | elem next xs = Just next
  | otherwise = findDupl (next:x:xs) tail
  where next = head + x

main = do
  content <- readFile("input.txt")
  let l = lines content
  let x = map readInt l
  let x' = cycle x
  print (fromJust (findDupl [0] x'))
