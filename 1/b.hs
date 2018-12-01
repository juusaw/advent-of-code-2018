import System.IO

readInt :: String -> Int
readInt ('+':rest) = read rest
readInt all = read all

findDupl :: Show a => Num a => Eq a => [a] -> [a] -> a
findDupl [] a = error "a"
findDupl a [] = error "b"
findDupl (x:xs) (head:tail)
  | elem next xs = next
  | otherwise = findDupl (next:x:xs) tail
  where next = head + x

main = do
  content <- readFile("input.txt")
  let l = lines content
  let x = map readInt l
  let x' = cycle x
  print (findDupl [0] x')
