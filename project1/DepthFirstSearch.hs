module DepthFirstSearch (listM) where

listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM f x = do rest <- listM' f [x]
               if null rest then return [x]
               else return $ x : rest

listM' :: (Eq a, Monad m) => (a -> m [a]) -> [a] -> m [a]
listM' f []     = return []
listM' f (x:xs) = do next <- listM' f [x]
                     rest <- listM' f xs
                     return $ x : next ++ rest
