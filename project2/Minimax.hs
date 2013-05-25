module Minimax where

import Data.List (minimumBy, maximumBy)

{-
 - This is the Minimax algorithm. The only source
 - used to assist in the implmentation is the 
 - materials from class.
 -
 - It was designed to be 100% reusable by any game
 - provided that a successor, utility, and
 - terminal test function is provided.
 -}

-- allow our types to be more descriptive
type Action = Int
type Utility = Int

{-
 - This is the main entry point of the algorithm.
 - It provides the integer value associated with
 - the highest utility action available.
 -}
minimaxDecision :: s                        -- the current state
                   -> (s -> [(Action, s)])  -- successor function
                   -> (s -> Utility)        -- utility function
                   -> (s -> Bool)           -- terminal test function
                   -> Action                -- return an action
minimaxDecision s succ u t = fst $ maxValue s succ u t

{-
 - This is the alternate entry point of the algorithm.
 - It provides the integer value associated with the
 - lowest utility action available.
 -
 - This was necessary due to the fact that player O
 - was required to always be the min player. Another
 - possible way to meet this requirement would have
 - been to include the current player in the state,
 - but this solution was determined to be simpler.
 -}
miniminDecision :: s
                   -> (s -> [(Action, s)])
                   -> (s -> Utility)
                   -> (s -> Bool)
                   -> Action
miniminDecision s succ u t = fst $ minValue s succ u t

{-
 - This function takes a state and returns both an
 - action and a utility. A minValue function calling
 - this recursively will use the utility value,
 - where a maximumDecision function calling this
 - function will use the action value.
 -}
maxValue :: s                        -- state
            -> (s -> [(Action, s)])  -- successor function
            -> (s -> Utility)        -- utility function
            -> (s -> Bool)           -- terminal test function
            -> (Action, Utility)     -- return an action and a utility
maxValue s succ u t
    | t s       = (0, u s)
    | otherwise = maximumBy compareUtility $ (0, minBound) : map actionUtility (succ s)
        where actionUtility (act, state) = (act, snd $ minValue state succ u t)

-- opposite of the maxValue function
minValue :: s
            -> (s -> [(Action, s)])
            -> (s -> Utility)
            -> (s -> Bool)
            -> (Action, Utility)
minValue s succ u t
    | t s       = (0, u s)
    | otherwise = minimumBy compareUtility $ (0, maxBound) : map actionUtility (succ s)
        where actionUtility (act, state) = (act, snd $ maxValue state succ u t)

-- a utility function which compares utility
-- between tuples
compareUtility :: (Action, Utility) -> (Action, Utility) -> Ordering
compareUtility (_, x) (_, y)
    | x > y     = GT
    | x < y     = LT
    | otherwise = EQ

