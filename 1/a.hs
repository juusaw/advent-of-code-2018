import System.IO

readInt :: String -> Int
readInt ('+':rest) = read rest
readInt all = read all

main = do
  content <- readFile("input.txt")
  let l = lines content
  let x = map readInt l
  let result = foldl (+) 0 x
  print result
