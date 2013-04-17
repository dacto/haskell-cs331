module BreadthFirstSearch (listM) where
import Data.List (find)


-- this function does not work because
-- the binding to 'tree' is an infinite
-- list and never returns
--
-- solveM :: (Eq a, Monad m) => (a -> m [a]) -> (a -> Bool) -> a -> m (Maybe a)
-- solveM expand isGoal root = do tree <- listM expand root
--                                return $ find isGoal tree



listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM expand node = do new <- expand node
                       rest <- listM' expand new
                       return $ node : rest

listM' :: (Eq a, Monad m) => (a -> m [a]) -> [a] -> m [a]
listM' expand row = do next <- mapM expand row
                       if null (concat next) then return []
                       else do rest <- listM' expand $ concat next
                               return $ row ++ rest
