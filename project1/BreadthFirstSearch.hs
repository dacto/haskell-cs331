-- Breadth First Search
module BreadthFirstSearch (solveM) where
import Data.List (find)

--     This is the primary code for breadth-first search.
--     It operates by continually expanding deeper rows of nodes,
--     supplying an infinite list of nodes. Find is called on
--     this list to locate the first node matching the goal.
solveM :: (Eq a, Functor m, Monad m) => (a -> m [a]) -> (a -> Bool) -> a -> m (Maybe a)
solveM expand isGoal root = fmap (find isGoal) (listM expand [root])

listM :: (Eq a, Monad m) => (a -> m [a]) -> [a] -> m [a]
listM expand row = do next <- mapM expand row
                      if null (concat next) then return []
                      else do rest <- listM expand $ concat next
                              return $ row ++ rest

