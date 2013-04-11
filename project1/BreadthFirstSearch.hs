module BreadthFirstSearch where

listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM f x = do next <- f x
               rest <- listM' f $ concat [next]
               return $ x : rest

listM' :: (Eq a, Monad m) => (a -> m [a]) -> [a] -> m [a]
listM' f x = do next <- mapM f x
                rest <- listM' f $ concat next
                return $ x ++ rest
