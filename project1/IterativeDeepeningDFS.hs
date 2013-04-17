module IterativeDeepeningDFS (solveM) where
import Data.Set (Set, empty, member, insert)
import Data.List (find)
import Control.Monad.Trans.State.Lazy



solveM :: Ord a =>
          ((a,[String]) -> [(a,[String])])
          -> ((a,[String]) -> Bool)
          -> (a,[String])
          -> [String]
solveM expand isGoal root =
    case runState (solveDepth expand isGoal root empty 0) (0, 0) of
    (Nothing, (nodes, depth))         -> ["no solution found"]++[show nodes++" nodes expanded"]++["searched to depth "++show depth]
    (Just (x, moves), (nodes, depth)) -> reverse moves++[show nodes++" nodes expanded"]++["searched to depth "++show depth]



solveDepth :: Ord a =>
              ((a,[String]) -> [(a,[String])])
              -> ((a,[String]) -> Bool)
              -> (a,[String])
              -> Set a
              -> Int
              -> State (Int, Int) (Maybe (a,[String]))
solveDepth expand isGoal root set searchDepth =
    do tree <- listM expand searchDepth set root
       (nodeCount, treeDepth) <- get
       let goal = find isGoal tree
       case goal of
            Nothing -> if searchDepth > treeDepth then return Nothing
                       else do nextDepth <- solveDepth expand isGoal root set $ searchDepth+1
                               return nextDepth
            _       -> return goal



listM :: Ord a =>
         ((a,[String]) -> [(a,[String])])
         -> Int
         -> Set a
         -> (a,[String])
         -> State (Int, Int) [(a,[String])]
listM expand searchDepth set node@(state, moves) =
    do let currDepth = length moves - 1
       if currDepth >= searchDepth || member state set then return [node]
       else do (nodeCount, treeDepth) <- get
               put (nodeCount + 1, max (currDepth + 1) treeDepth)
               let children = expand node
               let newSet = insert state set
               if null children then return [node]
               else do rest <- mapM (listM expand searchDepth newSet) children
                       return $ node : concat rest
