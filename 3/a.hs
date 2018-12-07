import System.IO
import Data.List
import Text.Regex

type Id = Int
type Coordinate = (Int, Int)
type Range = (Int, Int)
type Area = (Id, Coordinate, Range)

parse :: String -> Area
parse s = let (idStr:_:coordStr:rangeStr:_) = splitRegex (mkRegex " ") s
              id = read (tail idStr)
              [x, y] = map read (splitRegex (mkRegex ",") (init coordStr))
              [w, h] = map read (splitRegex (mkRegex "x") rangeStr)
            in (id, (x, y), (w, h))

areaToCoords :: Area -> [Coordinate]
areaToCoords (_,(x, y),(w, h)) = (,) <$> xs <*> ys
                                  where xs = [x..(x+w-1)]
                                        ys = [y..(y+h-1)]

main = do
  content <- readFile "input.txt"
  let l = lines content
  let areas = map parse l
  let covered = foldl (\l -> \a -> l ++ areaToCoords a) [] areas
  let groups = groupBy (==) (sort covered)
  print $ length $ filter ((<=) 2) $ map length $ groups
