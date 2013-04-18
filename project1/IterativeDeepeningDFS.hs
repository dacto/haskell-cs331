module IterativeDeepeningDFS (solveM) where
import Data.Set (Set, empty, member, insert)
import Data.List (find)
import MissionariesCannibals (Game, Lake, Moves)

solveM :: (Set Lake, Int, Int)      -- (history, max depth, nodes expanded)
          -> (Game -> [Game])       -- expand function
          -> (Game -> Bool)         -- isGoal function
          -> Int                    -- search depth
          -> [Game]                 -- children nodes
          -> (Maybe Game, Int, Int) -- return: (Game, Max Depth, Nodes Expanded)
solveM s _ _ _ [] = (Nothing, getDepth s, getNodes s)
solveM s e g d (x:xs)
    | member' x s               = solveM s e g d xs
    | g x                       = (Just x, getDepth s, getNodes s)
    | numMoves x == d           = solveM s e g d xs
    | otherwise                 =
        let (a, b, c) = solveM (insert' x s, max (getDepth s) (numMoves x), getNodes s+1) e g d (e x) in
        case a of Nothing -> solveM (getSet s, b, c) e g d xs
                  _       -> (a, b, c)

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

