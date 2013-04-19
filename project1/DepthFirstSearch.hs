-- Depth-First Search
module DepthFirstSearch (solveM) where
import Data.List (find)

--     This is the depth first search algorithm. Very simply put,
--     it recurses down the left side of the tree. Upon finding
--     a goal node or an invalid node, it stops recursion and returns.
solveM :: (Eq a, Monad m) => (a -> m [a]) -> (a -> Bool) -> a -> m (Maybe a)
solveM expand isGoal root = do tree <- listM expand root
                               return $ find isGoal tree

listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM expand node = do new <- expand node
                       if null new then return []
                       else do rest <- mapM (listM expand) new
                               return $ node : concat rest

