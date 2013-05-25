{-
 - This file contains all functions specific to TicTacToe.
 - Basically, everything except for the Minimax code. This
 - design was chosed to allow the Minimax algorithms to be
 - reusable if needed.
 -}

module TicTacToe where

import Minimax
import Data.List (transpose, find)
import Data.Char (intToDigit)

{-
 - some functions are borrowed from the examples at:
 - http://projects.haskell.org/operational/examples/TicTacToe.hs.html
 -
 - However, the majority of this code was rewritten, as I made a 
 - modification to the underlying data types.
 -
 - You can't learn much
 - about Haskell if you don't dig into the code.
 -}

-- My modified data types
data Mark = X | O | Empty Int deriving (Eq, Show)
type Board = [Mark]
data State = Game { board :: Board, turn :: Mark }
data Win = None | Draw | Winner Mark deriving (Eq, Show)

-- The successor function (required for Minimax)
successors :: State -> [(Int, State)]
successors (Game board turn) = concatMap applyMove [1..9]
    where applyMove n
            | board!!(n-1) == Empty n = [( n, (Game (a++[turn]++b) (other turn)) )]
            | otherwise             = []
                where a = take (n-1) board
                      b = drop n board
                      other m
                        | m == X    = O
                        | otherwise = X

-- The utility function (required for Minimax)
utility :: State -> Int
utility s
    | getWinner s == Winner X = 1
    | getWinner s == Winner O = -1
    | otherwise               = 0

-- The terminal test function (required for Minimax)
terminalTest :: State -> Bool
terminalTest state = if getWinner state == None then False else True

-- Used by the random player to return the list of possible moves
possibleMoves :: Board -> [Int]
possibleMoves board = map fst $ filter isValid $ zip [1..9] board
	where
	isValid (_, Empty _) = True
	isValid _            = False

-- Make a move on the board. Return the new board
-- if the move was valid.
move :: Int -> State -> Maybe State
move n s@(Game board turn)
    | board!!(n-1) == Empty n = Just ( Game (a++[turn]++b) (other turn) )
    | otherwise               = Nothing
        where a = take (n-1) board
              b = drop n board
              other m
                | m == X    = O
                | otherwise = X

-- Debugging tool which gets the advised move from Minimax
getAction :: State -> String
getAction state@(Game _ p)
    | p == X = "Advise move "++ show (minimaxDecision state successors utility terminalTest)
    | p == O = "Advise move "++ show (miniminDecision state successors utility terminalTest)

-- The empty board used at the beginning of the game
reset :: State
reset = Game newBoard X
    where newBoard = [Empty 1, Empty 2, Empty 3,
                      Empty 4, Empty 5, Empty 6,
                      Empty 7, Empty 8, Empty 9]

-- It's no GUI, but this board is the best I could get
-- Haskell to do for me.
prettyPrint :: Board -> IO ()
prettyPrint board =
    mapM_ putStrLn $ ["\n    |   |",
                      "  "++a++" | "++b++" | "++c,
                      "----+---+----",
                      "  "++d++" | "++e++" | "++f,
                      "----+---+----",
                      "  "++g++" | "++h++" | "++i,
                      "    |   |\n"]
    where [a,b,c,d,e,f,g,h,i] = map clean board
          clean X = "X"
          clean O = "O"
          clean (Empty x) = [intToDigit x]

-- Determine the winner of a given board
getWinner :: State -> Win
getWinner s@(Game board _) = case find isTicTacToe $ getRows board of
                                  Nothing -> if draw s then Draw else None
                                  Just (a,b,c) -> if all (== X) [a,b,c] then Winner X else Winner O

-- Determine if a board is in the draw state
draw :: State -> Bool
draw (Game board _) = all filled board
    where filled mark = mark == X || mark == O

-- Determine if a board has a tic-tac-toe
isTicTacToe :: (Mark, Mark, Mark) -> Bool
isTicTacToe (X, X, X) = True
isTicTacToe (O, O, O) = True
isTicTacToe _         = False

-- Deterine if someone has won
won :: State -> Bool
won (Game board _) = any isTicTacToe $ getRows board

-- Return the list of all "three in a row" possibilities
getRows :: [Mark] -> [(Mark, Mark, Mark)]
getRows [ a1, a2, a3,
          b1, b2, b3,
          c1, c2, c3 ] = [(a1, a2, a3),
                          (b1, b2, b3),
                          (c1, c2, c3),
                          (a1, b1, c1),
                          (a2, b2, c2),
                          (a3, b3, c3),
                          (a1, b2, c3),
                          (a3, b2, c1)]

