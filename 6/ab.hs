import Text.Regex
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Set as Set

type Coordinate = (Int, Int)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

parse :: String -> Coordinate
parse s = (x, y)
          where (x:y:_) = map read $ splitRegex (mkRegex ", ") s

distance :: Coordinate -> Coordinate -> Int
distance (a, b) (x, y) = abs (a - x) + abs (y - b)

findClosest :: [Coordinate] -> Coordinate -> Maybe Coordinate
findClosest xs c
  | distFrom closest < distFrom second = Just closest
  | otherwise = Nothing
  where distFrom = distance c
        (closest:second:_) = sortBy (comparing distFrom) xs

mostFrequentValue :: Ord a => [a] -> (a, Int)
mostFrequentValue [] = error "Can't get most frequent value of an empty list"
mostFrequentValue x = (head . head $ grouped, length . head $ grouped) 
                      where grouped = reverse . sortBy (comparing length) . group . sort $ x

getBounds :: [[Maybe Coordinate]] -> Set.Set Coordinate
getBounds grid = Set.fromList $ catMaybes $ bounds
                    where bounds = concat $ map (\f -> f grid) [head, last, map head, map last]

makeGrid :: Int -> Int -> (Coordinate -> a) -> [[a]]
makeGrid xLimit yLimit f = map (\x -> map (\y -> f (x, y)) [0..yLimit]) [0..xLimit]

main = do
  input <- readFile("input.txt")
  let l = lines input
  let coords = map parse l
  let maxDimensions = (maximum xs, maximum ys)
                        where xs = map fst coords
                              ys = map snd coords
  let (xLimit, yLimit) = mapPair ((*) 3) maxDimensions
  let grid = makeGrid xLimit yLimit $ findClosest coords
  let infinites = getBounds grid
  print $ snd $ mostFrequentValue $ filter (flip Set.notMember $ infinites) $ catMaybes $ concat grid
  let distGrid = makeGrid xLimit yLimit (\c -> foldl (\x co -> x + distance c co) 0 coords)
  print $ length $ filter ((>) 10000) $ concat distGrid
