import Control.Monad
import Data.Maybe
import Data.Time
import Data.Time.LocalTime
import Text.Regex
import Data.List (sortBy, groupBy, sort, group)
import Data.Function (on)
import qualified Data.Map as M

type TimedEvent = (LocalTime, Event)

data Event = WakesUp | FallsAsleep | BeginsShift Int deriving (Show, Eq)

parse :: String -> TimedEvent
parse s = (fromJust $ parseTimeStamp timeStr, parseEvent eventStr)
            where (timeStr:eventStr:_) = splitRegex (mkRegex "]") s

parseTimeStamp :: String -> Maybe LocalTime
parseTimeStamp s = parseTimeM False defaultTimeLocale "[%Y-%-m-%-d %H:%M" substr
          where substr = head $ splitRegex (mkRegex "]") s

parseEvent :: String -> Event
parseEvent " falls asleep" = FallsAsleep
parseEvent " wakes up" = WakesUp
parseEvent s = BeginsShift $ read $ head $ fromJust $ matchRegex (mkRegex "([0-9]+)") s

groupEvents :: [TimedEvent] -> [[TimedEvent]]
groupEvents = groupBy (\_ (_, e) -> e == WakesUp || e == FallsAsleep)

insertOrSum :: Int -> Int -> (M.Map Int Int) -> (M.Map Int Int)
insertOrSum k v m = M.insert k (v + M.findWithDefault 0 k m) m

insertOrAppend :: Int -> [a] -> (M.Map Int [a]) -> (M.Map Int [a])
insertOrAppend k v m = M.insert k (v ++ M.findWithDefault [] k m) m

toSleepTimes :: [[TimedEvent]] -> M.Map Int Int
toSleepTimes = foldl (\map ((_,BeginsShift x):eventList) -> insertOrSum x (getSleepTime eventList) map) M.empty

diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

diff :: LocalTime -> LocalTime -> Int
diff a b = floor $ diffLocalTime a b

getSleepTime :: [TimedEvent] -> Int
getSleepTime ((t1, FallsAsleep):(t2, WakesUp):rest) = (diff t2 t1) + getSleepTime rest
getSleepTime [] = 0
getSleepTime _ = error "Malformed input"

toMins :: LocalTime -> Int
toMins x = 60 * (todHour time) + todMin time
            where time = localTimeOfDay x

getSleepMins :: [TimedEvent] -> [Int]
getSleepMins ((t1, FallsAsleep):(t2, WakesUp):rest) = [toMins t1..((toMins t2) - 1)] ++ getSleepMins rest

getSleepMins [] = []
getSleepMins _ = error "Malformed input"

mostFrequentValue :: [Int] -> (Int, Int)
mostFrequentValue [] = (0, 0)
mostFrequentValue x = (head . head $ grouped, length . head $ grouped) 
                      where grouped = reverse . sortBy (compare `on` length) . group . sort $ x

main = do
  content <- readFile("input.txt")
  let l = lines content
  let events = groupEvents . sortBy (compare `on` fst) $ map parse l
  let sleeper = fst . head . reverse . sortBy (compare `on` snd) $ M.toList $ toSleepTimes $ events
  let minutes = getSleepMins $ join $ map tail $ filter (\((_,BeginsShift x):_) -> x == sleeper) events
  let optimalTime = mostFrequentValue minutes
  print $ sleeper * (fst optimalTime)
  let eventMap = foldl (\map ((_,BeginsShift x):eventList) -> insertOrAppend x eventList map) M.empty events
  let optimalGuard = head $ reverse . sortBy (compare `on` snd . snd) $ M.toList $ M.map (mostFrequentValue . getSleepMins) eventMap
  print $ (fst optimalGuard) * (fst . snd $ optimalGuard)
