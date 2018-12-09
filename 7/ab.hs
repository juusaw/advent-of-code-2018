import Text.Regex
import Data.Maybe
import qualified Data.Set as Set

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

main = do
  input <- readFile("input.txt")
  let l = lines input
  let deps = map parse l
  print $ solveDeps deps ['A'..'Z'] ""
