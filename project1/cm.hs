module Main where

import System.Environment
import System.IO
import Data.List (find)
import Data.Set (Set, member, insert, empty)
import Control.Monad.Trans.State.Lazy

type Moves = [String]
type Lake  = (Int, Int, Int, Int, Int, Int)
type Game  = (Lake, Moves)

supported :: [String]
supported = ["bfs"] -- ... ,"dfs", "iddfs", "astar"]

help :: String
help = "Usage:  cm <initial state file> <goal state file> <mode> <output file>\n\n"++
            "-h, --help  -> This usage document.\n"

solve :: String -> Lake -> Lake -> Moves
solve _ _ (_,_,c,_,_,z) | c<0 || z<0 || c+z /= 1 = ["Invalid boat configuration."]
solve m g l | m == "bfs" = reverse $ solveGame g (l, [show l])
            | otherwise  = ["Unsupported algorithm"]
            -- | m == "dfs"   = ...
            -- | m == "iddfs" = ...
            -- | m == "astar" = ...

solveGame :: Lake -> Game -> Moves
solveGame g ini = case find (isGoal g) $ zip [1..] $ evalState (bFSTrav [Just ini]) empty of
    Nothing              -> ["No solution."]
    Just (n, Just (_,m)) -> concat ["Nodes expanded: ",show n,"."]:m

isGoal :: Lake -> (Int, Maybe Game) -> Bool
isGoal _ (_, Nothing) = False
isGoal a (_, Just (b, _)) = a == b

bFSTrav :: [Maybe Game] -> State (Set Lake) [Maybe Game]
bFSTrav []  = return [Nothing]
bFSTrav x   = do nextRow  <- mapM expand x
                 restRows <- bFSTrav $ concat nextRow
                 return $ x ++ restRows

expand :: Maybe Game -> State (Set Lake) [Maybe Game]
expand Nothing              = return []
expand (Just game@(lake,_)) = do set <- get
                                 if member lake set then return []
                                 else do put $ insert lake set
                                         return [a, b, c, d, e]
                                         where a = move1M game
                                               b = move2M game
                                               c = move1C game
                                               d = move1M1C game
                                               e = move2C game

move1M :: Game -> Maybe Game
move1M ((a, b, 1, x, y, 0), moves)
    | a   < 1            = Nothing
    | x+1 < y            = Nothing
    | a-1 < b && a-1 > 0 = Nothing
    | otherwise          = Just ((a-1, b, 0, x+1, y, 1), "M --->":moves)
move1M ((a, b, 0, x, y, 1), moves)
    | x   < 1            = Nothing
    | a+1 < b            = Nothing
    | x-1 < y && x-1 > 0 = Nothing
    | otherwise          = Just ((a+1, b, 1, x-1, y, 0), "<--- M":moves)

move2M :: Game -> Maybe Game
move2M ((a, b, 1, x, y, 0), moves)
    | a   < 2            = Nothing
    | x+2 < y            = Nothing
    | a-2 < b && a-2 > 0 = Nothing
    | otherwise          = Just ((a-2, b, 0, x+2, y, 1), "MM--->":moves)
move2M ((a, b, 0, x, y, 1), moves)
    | x   < 2            = Nothing
    | a+2 < b            = Nothing
    | x-2 < y && x-2 > 0 = Nothing
    | otherwise          = Just ((a+2, b, 1, x-2, y, 0), "<---MM":moves)

move1C :: Game -> Maybe Game
move1C ((a, b, 1, x, y, 0), moves)
    | b   < 1            = Nothing
    | y+1 > x && x > 0   = Nothing
    | otherwise          = Just ((a, b-1, 0, x, y+1, 1), "C --->":moves)
move1C ((a, b, 0, x, y, 1), moves) 
    | y   < 1            = Nothing
    | b+1 > a && a > 0   = Nothing
    | otherwise          = Just ((a, b+1, 1, x, y-1, 0), "<--- C":moves)

move1M1C :: Game -> Maybe Game
move1M1C ((a, b, 1, x, y, 0), moves) 
    | a   < 1   = Nothing
    | b   < 1   = Nothing
    | x+1 < y+1 = Nothing
    | otherwise = Just ((a-1, b-1, 0, x+1, y+1, 1), "MC--->":moves)
move1M1C ((a, b, 0, x, y, 1), moves) 
    | x   < 1   = Nothing
    | y   < 1   = Nothing
    | a+1 < b+1 = Nothing
    | otherwise = Just ((a+1, b+1, 1, x-1, y-1, 0), "<---MC":moves)

move2C :: Game -> Maybe Game
move2C ((a, b, 1, x, y, 0), moves) 
    | b   < 2            = Nothing
    | y+2 > x && x > 0   = Nothing
    | otherwise          = Just ((a, b-2, 0, x, y+2, 1), "CC--->":moves)
move2C ((a, b, 0, x, y, 1), moves) 
    | y   < 2            = Nothing
    | b+2 > a && a > 0   = Nothing
    | otherwise          = Just ((a, b+2, 1, x, y-2, 0), "<---CC":moves)

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
