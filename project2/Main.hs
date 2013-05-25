{-# LANGUAGE GADTs, Rank2Types #-}
module Main where

-- This control program was taken almost completely from:
-- http://projects.haskell.org/operational/examples/TicTacToe.hs.html
--
-- I modified it to support the additional AI mode which operates
-- on the Minimax algorithm. I also had to rework the main function
-- a little.

import System.Environment (getArgs)
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
	
-- Here we match the arguments and execute the game with the
-- appropriate bots. I could not figure out how to get I/O
-- into the non-human players so playing those against each
-- other will simulate the game correctly but does not show
-- the boards along the way.
--
-- Note, however, that "Main minimax minimax" will always
-- result in a draw, but "Main random random" will indeed
-- have a random result.
main = do
	args <- getArgs
	g <- getStdGen
	case args of
		("human":"human":[])     -> runGame playerHuman playerHuman
		("human":"random":[])    -> runGame playerHuman (playerRandom g)
		("human":"minimax":[])   -> runGame playerHuman (playerAI_Y g)
		("random":"human":[])    -> runGame (playerRandom g) playerHuman
		("random":"random":[])   -> runGame (playerRandom g) (playerRandom g)
		("random":"minimax":[])  -> runGame (playerRandom g) (playerAI_Y g)
		("minimax":"human":[])   -> runGame (playerAI_X g) playerHuman
		("minimax":"random":[])  -> runGame (playerAI_X g) (playerRandom g)
		("minimax":"minimax":[]) -> runGame (playerAI_X g) (playerAI_Y g)
		_                   -> putStrLn "Main [human|random|minimax] [human|random|minimax]"

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

playerRandom :: Monad m => StdGen -> Player m ()
playerRandom = evalStateT ai
	where
	ai :: Monad m => StateT StdGen (ProgramT PlayerI m) ()
	ai = forever $ do
		board <- lift $ readBoard
		n     <- uniform (possibleMoves board)
		lift $ playMove n
		where
		uniform :: Monad m => [a] -> StateT StdGen m a
		uniform xs = do
			gen <- get
			let (n,gen') = randomR (1,length xs) gen
			put gen'
			return (xs !! (n-1))

-- I had to make an X bot and a Y bot since the only thing
-- they get to see is the board and don't really know which
-- player they are unless we tell them.
playerAI_X :: Monad m => StdGen -> Player m ()
playerAI_X = evalStateT ai
	where
	ai :: Monad m => StateT StdGen (ProgramT PlayerI m) ()
	ai = forever $ do
		board <- lift $ readBoard
		let n = minimaxDecision (Game board X) successors utility terminalTest
		lift $ playMove n

playerAI_Y :: Monad m => StdGen -> Player m ()
playerAI_Y = evalStateT ai
	where
	ai :: Monad m => StateT StdGen (ProgramT PlayerI m) ()
	ai = forever $ do
		board <- lift $ readBoard
		let n = miniminDecision (Game board O) successors utility terminalTest
		lift $ playMove n

