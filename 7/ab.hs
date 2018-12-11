import Text.Regex
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

type Task = Char

type Dependency = (Task, Task)

parse :: String -> Dependency
parse s = (head a, head b)
          where reg = mkRegex "Step ([A-Z]) must be finished before step ([A-Z]) can begin."
                (a:b:_) = fromJust $ matchRegex reg s

canComplete :: Set.Set Task -> [Dependency] -> Task -> Bool
canComplete completed deps task = null inCompleteDeps
                                  where taskDeps = filter (((==) task) . snd) deps
                                        inCompleteDeps = filter (\x -> Set.notMember (fst x) completed) taskDeps

solveDeps :: [Dependency] -> [Task] -> [Task] -> [Task]
solveDeps _ [] a = reverse a
solveDeps deps tasks completed = solveDeps deps remaining (next:completed)
                                where (next:_) = filter (canComplete (Set.fromList completed) deps) tasks
                                      remaining = filter ((/=) next) tasks

-- 60 + char position
getValue :: Char -> Int
getValue = (flip (-) 4) . fromEnum

numWorking :: Map.Map Task Int -> Int
numWorking x = length $ Map.filter (\x -> x > 0) x

nonNull :: Map.Map a Int -> Map.Map a Int
nonNull = Map.filter (\x -> x > 0)

inc :: Num a => a -> a
inc x = x + 1

filterKeys :: Eq a => [a] -> Map.Map a b -> Map.Map a b
filterKeys l m = Map.filterWithKey (\k _ -> elem k l) m

step_ :: [Dependency] -> Map.Map Task Int -> [Task] -> Int -> Int -> Int
step_ _ m c _ t | Map.null m = t
step_ deps tasks completed workers t = trace (show tasks') $ step_ deps tasks' completed' workers (inc t)
  where increasedTasks = Map.map inc $ nonNull tasks -- increase existing task completion
        finishedTasks = Map.filterWithKey (\k v -> v >= getValue k) tasks -- find ready tasks
        freeWorkers = workers - (numWorking tasks) + (length finishedTasks) -- count number of free workers
        completed' = (completed ++ Map.keys finishedTasks) -- update completed tasks list
        tasksWithoutCompleted = Map.filterWithKey (\k _ -> Map.notMember k finishedTasks) tasks
        availableTasks = map fst $ Map.toList $ Map.filterWithKey (\k _ -> canComplete (Set.fromList completed') deps k) tasksWithoutCompleted -- find tasks ready to start
        tasksToStart = take freeWorkers availableTasks -- select tasks to start
        updatedTasks = Map.union increasedTasks $ Map.map inc (filterKeys tasksToStart tasks) -- join tasks with work done, start work on new tasks
        --finishedTasks = Map.filterWithKey (\k v -> v == getValue k) updatedTasks
        newTasks = Map.union updatedTasks tasks
        tasks' = Map.filterWithKey (\k _ -> Map.notMember k finishedTasks) newTasks

--step :: [Dependency] -> Map.Map Task Int -> [Task] -> Int -> Int -> Int
--step _ m c _ 8 = c
step _ m c _ t | Map.null m = t
step deps tasks completed workers t = step deps nextTasks (completed ++ Map.keys finishedTasks) workers (inc t)
  where freeWorkers = (-) workers $ numWorking tasks
        availableTasks = map fst $ Map.toList $ Map.filterWithKey (\k _ -> canComplete (Set.fromList completed) deps k) tasks
        tasksToStart = take freeWorkers availableTasks
        updatedTasks = Map.map inc $ Map.union (nonNull tasks) (filterKeys tasksToStart tasks)
        finishedTasks = Map.filterWithKey (\k v -> v == getValue k) updatedTasks
        newTasks = Map.union updatedTasks tasks
        nextTasks = Map.filterWithKey (\k _ -> Map.notMember k finishedTasks) newTasks

main = do
  input <- readFile("input.txt")
  let l = lines input
  let deps = map parse l
  print $ solveDeps deps ['A'..'Z'] ""
  print $ step deps (Map.fromList $ zip ['A'..'Z'] (repeat 0)) "" 5 (-2)
