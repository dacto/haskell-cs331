module IterativeDeepeningDFS (listM) where

import MissionariesCannibals (Game, Lake, Moves)

listM :: (Monad m) => (Game -> m [Game]) -> Game -> m [Game]
listM f x = do
               val <- listM' f x 1
               return val

listM' :: (Monad m) => (Game -> m [Game]) -> Game -> Int -> m [Game]
listM' f x@(_,n) i = do fiveMoves <- f x
                        let depth = length n
                        if null fiveMoves || depth >= i then return []
                        else do next <- mapM (\v -> listM' f v i) fiveMoves
                                return $ x : concat next
