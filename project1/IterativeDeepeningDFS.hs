module IterativeDeepeningDFS (solveM) where
import Data.List (find)
import Control.Monad.Trans.State.Lazy



solveM :: Eq a => (a -> Int -> State (Int, Int) [a]) -> (a -> Bool) -> a -> State (Int, Int) (Maybe a)
solveM expand isGoal root = do solution <- solveDepth expand isGoal root 1
                               return solution


solveDepth :: Eq a => (a -> Int -> State (Int, Int) [a]) -> (a -> Bool) -> a -> Int -> State (Int, Int) (Maybe a)
solveDepth expand isGoal root depth =
    do tree <- listM expand root depth
       (count, maxDepth) <- get
       let search = find isGoal tree
       case search of
            Nothing -> if depth > maxDepth then return Nothing
                       else do solution <- solveDepth expand isGoal root (depth+1)
                               return solution
            _       -> return search




listM :: Eq a => (a -> Int -> State (Int, Int) [a]) -> a -> Int -> State (Int, Int) [a]
listM expand node depth = do new <- expand node depth
                             if null new then return []
                             else do rest <- mapM (\l -> listM expand l depth) new
                                     return $ node : concat rest

