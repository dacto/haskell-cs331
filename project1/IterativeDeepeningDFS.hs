module IterativeDeepeningDFS (listM) where

listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM f x = return $ listM' f x 1

listM' :: (Eq a, Monad m) => (a -> m [a]) -> a -> Int -> m [a]
listM' f x@(_,n) i = do expand <- f x
                        let depth = length n
                        if null expand || depth >= i then return []
                        else do next <- mapM (\v -> listM' f v i) expand
                                return $ x : concat next
