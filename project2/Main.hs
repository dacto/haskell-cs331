module Main where

-- This control program was taken almost completely from:
-- http://projects.haskell.org/operational/examples/TicTacToe.hs.html
--

import TicTacToe
import Minimax
import Control.Monad
import Control.Monad.Operational
import Control.Monad.State

import Data.Either
import Data.List

import System.Random

data PlayerI a where
    ReadBoard :: PlayerI Board
    PlayMove  :: Int -> PlayerI Bool
    
type Player m a = ProgramT PlayerI m a

readBoard = singleton ReadBoard
playMove  = singleton . PlayMove

    -- interpreter
runGame :: Player IO () -> Player IO () -> IO ()
runGame player1 player2 = eval' reset player1 player2
    where
    eval' game p1 p2 = viewT p1 >>= \p1view -> eval game p1view p2
    
    eval :: TicTacToe.State
         -> ProgramViewT PlayerI IO () -> Player IO ()
         -> IO ()
    eval game (Return _)            _  = return ()
    eval game (ReadBoard   :>>= p1) p2 = eval' game (p1 (board game)) p2
    eval game (PlayMove mv :>>= p1) p2 =
        case move mv game of
            Nothing         -> eval' game (p1 False) p2
            Just game'
                | won game' -> let p = turn game in
                               putStrLn $ "Player " ++ show p ++ " has won!"
                | draw game'-> putStrLn $ "It's a draw."
                | otherwise -> eval' game' p2 (p1 True)
    
main = do
    g <- getStdGen
    runGame playerHuman (playerAI g)

playerHuman :: Player IO ()
playerHuman = forever $ readBoard >>= liftIO . prettyPrint >> doMove
    where
    doMove :: Player IO ()
    doMove = do
        liftIO . putStrLn $ "At which number would you like to play?"
        n <- liftIO getLine
        b <- playMove (read n)
        unless b $ do
            liftIO . putStrLn $ "Position " ++ show n ++ " is already full."
            doMove


playerAI :: Monad m => StdGen -> Player m ()
playerAI = evalStateT ai
    where
    ai :: Monad m => StateT StdGen (ProgramT PlayerI m) ()
    ai = forever $ do
        board <- lift $ readBoard
        let n = miniminDecision (Game board O) successors utility terminalTest
        lift $ playMove n
