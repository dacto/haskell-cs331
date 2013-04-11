module DepthFirstSearch (listM) where

listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM f x = do rest <- listM' f x
               if nulll rest then return x
               else return $ x : rest

listM' :: (Eq a, Monad m) => (a -> m [a]) -> [a] -> m [a]
listM' f []     = return []
listM' f (x:xs) = return $ x : listM' f x ++ listM' f xs
