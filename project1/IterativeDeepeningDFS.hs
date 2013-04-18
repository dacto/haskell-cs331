module IterativeDeepeningDFS (solveM) where
import Data.Set (Set, empty, member, insert)
import Data.List (find)
import MissionariesCannibals (Game, Lake, Moves)

solveM :: (Set Lake, Int, Int)      -- (history, max depth, nodes expanded)
          -> (Game -> [Game])       -- expand function
          -> (Game -> Bool)         -- isGoal function
          -> Int                    -- first search depth
          -> [Game]                 -- children nodes
          -> (Maybe Game, Int, Int) -- return: (Game, Max Depth, Nodes Expanded)
solveM  s e g d n = let (a, b, c) = solveDepth s e g d n in
                    case a of Nothing -> if b < d then (a, b, c)
                                         else solveM (empty, 0, c) e g (d+1) n
                              _       -> (a, b, c)



solveDepth :: (Set Lake, Int, Int)      -- (history, max depth, nodes expanded)
          -> (Game -> [Game])       -- expand function
          -> (Game -> Bool)         -- isGoal function
          -> Int                    -- current search depth
          -> [Game]                 -- children nodes
          -> (Maybe Game, Int, Int) -- return: (Game, Max Depth, Nodes Expanded)
solveDepth s _ _ _ [] = (Nothing, getDepth s, getNodes s)
solveDepth s e g d (x:xs)
    | member' x s               = solveDepth s e g d xs
    | g x                       = (Just x, max (getDepth s) (numMoves x), getNodes s)
    | numMoves x == d           = solveDepth s e g d xs
    | otherwise                 =
        let (a, b, c) = solveDepth (insert' x s, max (getDepth s) (numMoves x+1), getNodes s+1) e g d (e x) in
        case a of Nothing -> solveDepth (getSet s, max (getDepth s) b, c) e g d xs
                  _       -> (a, max (getDepth s) b, c)

insert' :: Game -> (Set Lake, Int, Int) -> Set Lake
insert' (l, _) (s, _, _) = insert l s

member' :: Game -> (Set Lake, Int, Int) -> Bool
member' (l, _) (s, _, _) = member l s

getDepth :: (Set Lake, Int, Int) -> Int
getDepth (_, x, _) = x

getNodes :: (Set Lake, Int, Int) -> Int
getNodes (_, _, x) = x

getSet :: (Set Lake, Int, Int) -> Set Lake
getSet (x, _, _) = x

numMoves :: Game -> Int
numMoves (lake, moves) = length moves - 1

