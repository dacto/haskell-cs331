module DepthFirstSearch (listM) where

listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM f x = do expand <- f x
               if null expand then return []
               else do next <- mapM (listM f) expand
                       return $ x : concat next
