module IterativeDeepeningDFS (listM) where

listM :: (Eq a, Eq b, Monad m) => ((a,[b]) -> m [(a,[b])]) -> (a,[b]) -> m [(a,[b])]
listM f x = listM' f x 1

listM' :: (Eq a, Eq b, Monad m) => ((a,[b]) -> m [(a,[b])]) -> (a,[b]) -> Int -> m [(a,[b])]
listM' f x@(_,n) i = do expand <- f x
                        let depth = length n
                        if null expand || depth >= i then return []
                        else do next <- mapM (\v -> listM' f v i) expand
                                return $ x : concat next
