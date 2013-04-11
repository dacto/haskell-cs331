module Test where

import Data.Set (Set, empty)
import Control.Monad.Trans.State.Lazy
import BreadthFirstSearch as BFS
import DepthFirstSearch as DFS
import MissionariesCannibals as MC

-- Outputs the nodes expanded.
-- *GHCi Only* Usage: test mode goalTuple initialLake
test :: String -> Lake -> Lake -> [(Int, Game)]
test m g l | m == "bfs" = reverse $ test' BFS.listM g (l, [show l])
           | m == "dfs" = reverse $ test' DFS.listM g (l, [show l])

test' :: ((Game -> State (Set Lake) [Game]) -> Game -> State (Set Lake) [Game]) -> Lake -> Game -> [(Int, Game)]
test' f g ini = takeWhile (MC.isNotGoal g) $ zip [1..] $ evalState (f MC.expand ini) empty