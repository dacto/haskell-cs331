module Main where

import System.Environment
import System.IO
import Data.List (find)
import Data.Set (Set, empty)
import Control.Monad.Trans.State.Lazy
import BreadthFirstSearch as BFS
import DepthFirstSearch as DFS
import MissionariesCannibals as MC

supported :: [String]
supported = ["bfs","dfs"]-- ... "iddfs", "astar"]

help :: String
help = "Usage:  main <initial state file> <goal state file> <mode> <output file>\n\n"++
            "-h, --help  -> This usage document.\n"

-- 
solve :: String -> Lake -> Lake -> Moves
solve _ _ (_,_,c,_,_,z) | c<0 || z<0 || c+z /= 1 = ["Invalid boat configuration."]
solve m g l | m == "bfs" = reverse $ solveGame BFS.listM g (l, [show l])
            | m == "dfs" = reverse $ solveGame DFS.listM g (l, [show l])
            | otherwise  = ["Unsupported algorithm"]

            -- | m == "iddfs" = ...
            -- | m == "astar" = ...

solveGame :: ((Game -> State (Set Lake) [Game]) -> Game -> State (Set Lake) [Game]) -> Lake -> Game -> Moves
solveGame f g ini = case find (MC.isGoal g) $ zip [1..] $ evalState (f MC.expand ini) empty of
    Nothing         -> ["No solution."]
    Just (n, (_,m)) -> concat ["Nodes expanded: ",show n,"."]:m

-- Outputs the nodes expanded.
-- *GHCi Only* Usage: test mode goalTuple initialLake
test :: String -> Lake -> Lake -> [(Int, Game)]
test m g l | m == "bfs" = reverse $ test' BFS.listM g (l, [show l])
           | m == "dfs" = reverse $ test' DFS.listM g (l, [show l])

test' :: ((Game -> State (Set Lake) [Game]) -> Game -> State (Set Lake) [Game]) -> Lake -> Game -> [(Int, Game)]
test' f g ini = takeWhile (MC.isNotGoal g) $ zip [1..] $ evalState (f MC.expand ini) empty

main :: IO ()
main = do 
        args <- getArgs
        case args of
            (w:x:y:z:[]) -> if y `elem` supported
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
