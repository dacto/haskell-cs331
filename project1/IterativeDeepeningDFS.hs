module IterativeDeepeningDFS (solveM) where
import Data.List (find)
import Control.Monad.Trans.State.Lazy



solveM :: Eq a=>
          ((a,[String]) -> [(a,[String])])
          -> ((a,[String]) -> Bool)
          -> (a,[String])
          -> [String]
solveM expand isGoal root =
    case runState (solveDepth expand isGoal root 1) (0, 0) of
    (Nothing, (nodes, depth))         -> ["no solution found"]++[show nodes++" nodes expanded"]++["searched to depth "++show depth]
    (Just (x, moves), (nodes, depth)) -> reverse moves++[show nodes++" nodes expanded"]++["searched to depth "++show depth]



solveDepth :: Eq a =>
              ((a,[String]) -> [(a,[String])])
              -> ((a,[String]) -> Bool)
              -> (a,[String])
              -> Int
              -> State (Int, Int) (Maybe (a,[String]))
solveDepth expand isGoal root searchDepth =
    do tree <- listM expand searchDepth root
       (nodeCount, treeDepth) <- get
       let goal = find isGoal tree
       case goal of
            Nothing -> if searchDepth > treeDepth then return Nothing
                       else do nextDepth <- solveDepth expand isGoal root $ searchDepth+1
                               return nextDepth
            _       -> return goal



listM :: Eq a =>
         ((a,[String]) -> [(a,[String])])
         -> Int
         -> (a,[String])
         -> State (Int, Int) [(a,[String])]
listM expand searchDepth node@(state, moves) =
    do let currDepth = length moves
       if currDepth > searchDepth then return []
       else do let children = expand node
               if null children then return []
               else do (nodeCount, treeDepth) <- get
                       put (nodeCount+1, max currDepth treeDepth)
                       rest <- mapM (listM expand searchDepth) children
                       return $ node : concat rest
