module Minimax where

import Data.List (minimumBy, maximumBy)

type Action = Int
type Utility = Int

minimaxDecision :: s
                   -> (s -> [(Action, s)])
                   -> (s -> Utility)
                   -> (s -> Bool)
                   -> Action
minimaxDecision s succ u t = fst $ maxValue s succ u t

miniminDecision :: s
                   -> (s -> [(Action, s)])
                   -> (s -> Utility)
                   -> (s -> Bool)
                   -> Action
miniminDecision s succ u t = fst $ minValue s succ u t

maxValue :: s
            -> (s -> [(Action, s)])
            -> (s -> Utility)
            -> (s -> Bool)
            -> (Action, Utility)
maxValue s succ u t
    | t s       = (0, u s)
    | otherwise = maximumBy compareUtility $ (0, minBound) : map actionUtility (succ s)
        where actionUtility (act, state) = (act, snd $ minValue state succ u t)

minValue :: s
            -> (s -> [(Action, s)])
            -> (s -> Utility)
            -> (s -> Bool)
            -> (Action, Utility)
minValue s succ u t
    | t s       = (0, u s)
    | otherwise = minimumBy compareUtility $ (0, maxBound) : map actionUtility (succ s)
        where actionUtility (act, state) = (act, snd $ maxValue state succ u t)

compareUtility :: (Action, Utility) -> (Action, Utility) -> Ordering
compareUtility (_, x) (_, y)
    | x > y     = GT
    | x < y     = LT
    | otherwise = EQ

