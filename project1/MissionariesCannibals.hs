module MissionariesCannibals where
import Data.Set (Set, member, insert)
import Control.Monad.Trans.State.Lazy

type Moves = [String]
type Lake = (Int, Int, Int, Int, Int, Int)
type Game = (Lake, Moves)



isGoal :: Lake -> Game -> Bool
isGoal a (b, _) = a == b


getDepth :: Game -> Int
getDepth (_, m) = length m


basicExpand :: Game -> [Game]
basicExpand game = concat [a, b, c, d, e]
               where a = move1M game
                     b = move2M game
                     c = move1C game
                     d = move1M1C game
                     e = move2C game


expand' :: Game -> State (Int, Int, Set Lake) [Game]
expand' game@(lake,_) =
    do (count, maxDepth, set) <- get
       if member lake set then return []
       else do put (count+1, maxDepth, insert lake set)
               return $ basicExpand game



expand :: Game -> State (Int, Int, Set Lake) [Game]
expand game@(lake,_) =
    do (count, maxDepth, set) <- get
       let currDepth = getDepth game
       if member lake set then return []
       else do let depth = max currDepth maxDepth
               put (count+1, depth, insert lake set)
               return $ basicExpand game



expandToDepth :: Game -> Int -> State (Int, Int) [Game]
expandToDepth game@(lake,_) depth =
    do (count, maxDepth) <- get
       let currDepth = getDepth game
       if currDepth > depth then return []
       else do let depth = max currDepth maxDepth
               put (count+1, depth)
               return $ basicExpand game



move1M :: Game -> [Game]
move1M ((a, b, 1, x, y, 0), moves)
    | a   < 1            = []
    | x+1 < y            = []
    | a-1 < b && a-1 > 0 = []
    | otherwise          = [((a-1, b, 0, x+1, y, 1), "M --->":moves)]
move1M ((a, b, 0, x, y, 1), moves)
    | x   < 1            = []
    | a+1 < b            = []
    | x-1 < y && x-1 > 0 = []
    | otherwise          = [((a+1, b, 1, x-1, y, 0), "<--- M":moves)]

move2M :: Game -> [Game]
move2M ((a, b, 1, x, y, 0), moves)
    | a   < 2            = []
    | x+2 < y            = []
    | a-2 < b && a-2 > 0 = []
    | otherwise          = [((a-2, b, 0, x+2, y, 1), "MM--->":moves)]
move2M ((a, b, 0, x, y, 1), moves)
    | x   < 2            = []
    | a+2 < b            = []
    | x-2 < y && x-2 > 0 = []
    | otherwise          = [((a+2, b, 1, x-2, y, 0), "<---MM":moves)]

move1C :: Game -> [Game]
move1C ((a, b, 1, x, y, 0), moves)
    | b   < 1            = []
    | y+1 > x && x > 0   = []
    | otherwise          = [((a, b-1, 0, x, y+1, 1), "C --->":moves)]
move1C ((a, b, 0, x, y, 1), moves) 
    | y   < 1            = []
    | b+1 > a && a > 0   = []
    | otherwise          = [((a, b+1, 1, x, y-1, 0), "<--- C":moves)]

move1M1C :: Game -> [Game]
move1M1C ((a, b, 1, x, y, 0), moves) 
    | a   < 1   = []
    | b   < 1   = []
    | x+1 < y+1 = []
    | otherwise = [((a-1, b-1, 0, x+1, y+1, 1), "MC--->":moves)]
move1M1C ((a, b, 0, x, y, 1), moves) 
    | x   < 1   = []
    | y   < 1   = []
    | a+1 < b+1 = []
    | otherwise = [((a+1, b+1, 1, x-1, y-1, 0), "<---MC":moves)]

move2C :: Game -> [Game]
move2C ((a, b, 1, x, y, 0), moves) 
    | b   < 2            = []
    | y+2 > x && x > 0   = []
    | otherwise          = [((a, b-2, 0, x, y+2, 1), "CC--->":moves)]
move2C ((a, b, 0, x, y, 1), moves) 
    | y   < 2            = []
    | b+2 > a && a > 0   = []
    | otherwise          = [((a, b+2, 1, x, y-2, 0), "<---CC":moves)]
