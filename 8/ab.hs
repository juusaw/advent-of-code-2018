import Text.Regex
import Data.Maybe

parse :: String -> [Int]
parse s = map read $ splitRegex (mkRegex " ") s

takeLast :: Int -> [a] -> [a]
takeLast c s = drop ((length s) - c) s

dropLast :: Int -> [a] -> [a]
dropLast c s = take ((length s) - c) s

data Node = Node [Node] [Int] deriving Show

parseNode :: [Int] -> Int -> ([Node], [Int])
parseNode [] x = ([], [])
parseNode a 0 = ([], a)
parseNode (0:metaCount:rest) 1 = ([Node [] $ take metaCount rest], drop metaCount rest)
parseNode (childCount:metaCount:rest) 1 = let (node, newRest) = parseNode rest childCount
                                          in ([Node node $ take metaCount newRest], drop metaCount newRest)
parseNode (children:meta:rest) rem = let (node, newRest) = parseNode (children:meta:rest) 1
                                         (moreNodes, trueRest) = parseNode newRest (rem - 1)
                                     in (node ++ moreNodes, trueRest)

flattenNode :: [Node] -> [Int]
flattenNode [] = []
flattenNode (Node children meta:rest) = meta ++ flattenNode children ++ flattenNode rest

safeFindIndex :: Int -> [a] -> Maybe a
safeFindIndex x [] = Nothing
safeFindIndex 1 (x:xs) = Just x
safeFindIndex n (x:xs) = safeFindIndex (n - 1) xs

calculateNodeValue :: [Node] -> [Int]
calculateNodeValue [] = []
calculateNodeValue (Node [] meta:xs) = meta ++ calculateNodeValue xs
calculateNodeValue (Node children meta:xs) = let validChildren = catMaybes $ map (flip safeFindIndex children) meta
                                             in calculateNodeValue validChildren ++ calculateNodeValue xs

main = do
  content <- readFile("input.txt")
  let input = parse content 
  let rootNode = fst $ parseNode input 1
  let metaList = flattenNode rootNode
  print $ sum metaList
  let indexedValues = calculateNodeValue rootNode
  print $ sum indexedValues
