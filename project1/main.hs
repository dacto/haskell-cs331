module Main where

import System.Environment
import System.IO
import Data.Set (empty)
import Control.Monad.Trans.State.Lazy
import BreadthFirstSearch as BFS
import DepthFirstSearch as DFS
import IterativeDeepeningDFS as IFS
import AStarSearch as ASS
import MissionariesCannibals


calc (Nothing, (n, _, _)) =
    reverse $ (show n ++ " nodes expanded") : ["no solution found"]
calc (Just (_, m), (n, _, _)) =
    reverse $ (show n ++ " nodes expanded") : m

calc' (Nothing, (n, _)) =
    reverse $ (show n ++ " nodes expanded") : ["no solution found"]
calc' (Just (_, m), (n, _)) =
    reverse $ (show n ++ " nodes expanded") : m



solve :: String -> Lake -> Game -> Moves
solve "bfs" goal start =
    calc $ runState (BFS.solveM expand (isGoal goal) start) (0, 0, empty)
solve "dfs" goal start =
    calc $ runState (DFS.solveM expand (isGoal goal) start) (0, 0, empty)
solve "iddfs" goal start =
    [show $ IFS.solveM (empty, 0, 0) basicExpand (isGoal goal) 2 [start]]
solve "astar" goal start = 
    calc $ runState (ASS.solveM heuristic (isGoal goal) start) (0, 0, empty)




main :: IO ()
main = do 
        args <- getArgs
        case args of
            (w:x:y:z:[]) -> if supported y
                                then do
                                  start <- openFile w ReadMode
                                  goal  <- openFile x ReadMode
                                  out   <- openFile z WriteMode
                                  mainsub start goal y out
                                  hClose start
                                  hClose goal
                                  hClose out
                                else putStrLn help
            _            -> putStrLn help



mainsub :: Handle -> Handle -> String -> Handle -> IO ()
mainsub start goal mode output = do
                    a <- hGetLine start
                    b <- hGetLine start
                    c <- hGetLine goal
                    d <- hGetLine goal
                    let startState = createtuple a b
                    let goalState = createtuple c d
                    let results = if goodBoats startState && supported mode
                                  then solve mode goalState (startState, [show startState])
                                  else [help]
                    mapM_ (hPutStrLn output) results
                    mapM_ putStrLn results



split' :: (Char -> Bool) -> String -> [String]
split' p s = case dropWhile p s of "" -> []
                                   s' -> w : split' p s''
                                    where (w, s'') = break p s'



createtuple :: String -> String -> Lake
createtuple x y = tuplefy $ split' (==',') x ++ split' (==',') y



tuplefy :: [String] -> Lake
tuplefy [a,b,c,x,y,z] = (read a::Int, read b::Int,read c::Int, read x::Int,read y::Int, read z::Int)



supported :: String -> Bool
supported y = y `elem` ["bfs","dfs","iddfs", "astar"]



help :: String
help = "Usage:  main <initial state file> <goal state file> <mode> <output file>\n\n"++
       "        Valid modes:\n"++
       "            bfs   = Breadth First Search\n"++
       "            dfs   = Depth First Search\n"++
       "            iddfs = Itterative Deepening Depth First Search\n"++
       "            astar = AStar Search\n\n"++
       "        Options:\n"++
       "            -h, --help  -> This usage document.\n"



goodBoats :: Lake -> Bool
goodBoats (_,_,c,_,_,z) = c >= 0 && z >= 0 && c + z == 1
