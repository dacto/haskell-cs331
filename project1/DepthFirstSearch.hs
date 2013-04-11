module DepthFirstSearch (listM) where

listM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m [a]
listM f x = do rest <- listM' f [x]
               return (if null rest then [x] else x : rest)

listM' :: (Eq a, Monad m) => (a -> m [a]) -> [a] -> m [a]
listM' _ []     = return []
listM' f (x:xs) = do next <- listM' f [x]
                     rest <- listM' f xs
                     return $ x : next ++ rest
