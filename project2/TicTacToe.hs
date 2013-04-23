module TicTacToe where

import Minimax
import Data.List (transpose, find)
import Data.Char (intToDigit)

-- some concepts are borrowed from:
-- http://projects.haskell.org/operational/examples/TicTacToe.hs.html
data Mark = X | O | Empty Int deriving (Eq, Show)
type Board = [Mark]
data State = Game { board :: Board, turn :: Mark }
data Win = None | Draw | Winner Mark deriving (Eq, Show)

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

move :: Int -> State -> Maybe State
move n s@(Game board turn)
    | board!!(n-1) == Empty n = Just ( Game (a++[turn]++b) (other turn) )
    | otherwise               = Nothing
        where a = take (n-1) board
              b = drop n board
              other m
                | m == X    = O
                | otherwise = X

utility :: State -> Int
utility s
    | getWinner s == Winner X = 1
    | getWinner s == Winner O = -1
    | otherwise               = 0

getAction :: State -> String
getAction state@(Game _ p)
    | p == X = "Advise move "++ show (minimaxDecision state successors utility terminalTest)
    | p == O = "Advise move "++ show (miniminDecision state successors utility terminalTest)

getActions :: State -> String
getActions state@(Game _ p)
    | p == X = "Advise moves "++ show (minimaxDecisions state successors utility terminalTest)
    | p == O = "Advise moves "++ show (miniminDecisions state successors utility terminalTest)

reset :: State
reset = Game newBoard X
    where newBoard = [Empty 1, Empty 2, Empty 3,
                      Empty 4, Empty 5, Empty 6,
                      Empty 7, Empty 8, Empty 9]

prettyPrint :: Board -> IO ()
prettyPrint board =
    mapM_ putStrLn $ ["+---+---+---+",
                      "| "++a++" | "++b++" | "++c++" |",
                      "+---+---+---+",
                      "| "++d++" | "++e++" | "++f++" |",
                      "+---+---+---+",
                      "| "++g++" | "++h++" | "++i++" |",
                      "+---+---+---+"]
                      --result turn,
                      --getActions s,
                      --getAction s]
    where [a,b,c,d,e,f,g,h,i] = map clean board
          --result turn
          --  | getWinner s == Winner X = "Winner: X"
          --  | getWinner s == Winner O = "Winner: O"
          --  | otherwise               = "Turn: Player " ++ show turn
          clean X = "X"
          clean O = "O"
          clean (Empty x) = [intToDigit x]

terminalTest :: State -> Bool
terminalTest state = if getWinner state == None then False else True

getWinner :: State -> Win
getWinner s@(Game board _) = case find isTicTacToe $ getRows board of
                                  Nothing -> if draw s then Draw else None
                                  Just (a,b,c) -> if all (== X) [a,b,c] then Winner X else Winner O

draw :: State -> Bool
draw (Game board _) = all filled board
    where filled mark = mark == X || mark == O

isTicTacToe :: (Mark, Mark, Mark) -> Bool
isTicTacToe (X, X, X) = True
isTicTacToe (O, O, O) = True
isTicTacToe _         = False

won :: State -> Bool
won (Game board _) = any isTicTacToe $ getRows board

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

