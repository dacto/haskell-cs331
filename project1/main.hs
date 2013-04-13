module Main where

import System.Environment
import System.IO
import Data.List (find)
import Data.Set (Set, empty)
import Control.Monad.Trans.State.Lazy
import BreadthFirstSearch as BFS
import DepthFirstSearch as DFS
import IterativeDeepeningDFS as IFS
import MissionariesCannibals

calc (Nothing, (n, _))     = reverse $ (show n ++ " nodes expanded") : ["no solution found"]
calc (Just (_, m), (n, _)) = reverse $ (show n ++ " nodes expanded") : m



solve :: String -> Lake -> Lake -> Moves
solve mode goal lake
    | badBoats lake   = ["Invalid boat configuration"]
    | mode == "bfs"   = reverse $ solve' BFS.listM goal start
    | mode == "dfs"   = calc $ runState (DFS.solveM expand' (isGoal' goal) start) (0, empty)
    | mode == "iddfs" = reverse $ solve' IFS.listM goal start
    | mode == "astar" = ["Algorithm incomplete"]
    | otherwise       = ["Unsupported algorithm"]
        where start = (lake, [show lake])



solve' :: ((Game -> State (Set Lake) [Game]) -> Game -> State (Set Lake) [Game]) -> Lake -> Game -> Moves
solve' f g i = case find (isGoal g) $ zip [1..] $ evalState (f expand i) empty of
              Nothing         -> ["No solution."]
              Just (n, (_,m)) -> concat ["Nodes expanded: ",show n,"."]:m



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
                    mapM_ (hPutStrLn output) $ solve mode (createtuple c d) (createtuple a b)



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



badBoats :: Lake -> Bool
badBoats (_,_,c,_,_,z) = c<0 || z<0 || c+z /= 1
