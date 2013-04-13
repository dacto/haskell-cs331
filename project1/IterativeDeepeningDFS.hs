module IterativeDeepeningDFS (listM) where

listM :: (Eq a, Eq b, Monad m) => ((a,[b]) -> m [(a,[b])]) -> (a,[b]) -> m [(a,[b])]
listM f x = listDepth f x 20

listDepth :: (Eq a, Eq b, Monad m) => ((a,[b]) -> m [(a,[b])]) -> (a,[b]) -> Int -> m [(a,[b])]
listDepth f x@(_,n) i
    | length n > i = return []
    | otherwise    = do moves <- f x
                        if null moves then return []
                        else do next <- mapM (\v -> listDepth f v i) moves
                                return $ x : concat next
