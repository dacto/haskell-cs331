module AStarSearch (solveM) where
import Data.PQueue.Prio.Min as PQ
import MissionariesCannibals
import Control.Monad.Trans.State.Lazy
import Data.Set as S (Set, member, insert)
import Data.List (find)

solveM :: (Game -> Int) -> (Game -> Bool) -> Game -> State (Int, Int, Set Lake) (Maybe Game)
solveM heu isGoul root = do tree <- listM heu root
                            return $ find isGoul tree

listM :: (Game -> Int) -> Game -> State (Int, Int, Set Lake) [Game]
listM heu node = do new <- pExpand heu node
                    if PQ.null new then return []
                    else do rest <- mapM (listM heu) (PQ.elems new)
                            return $ node : concat rest

pExpand :: (Game -> Int) -> Game -> State (Int, Int, Set Lake) (MinPQueue Int Game)
pExpand heu g@(l,_) = do (count, maxDepth, set) <- get
                         let currDepth = getDepth g
                         if member l set then return empty
                         else do let depth = max currDepth maxDepth
                                 put (count+1, depth, S.insert l set)
                                 return $ priorityMoves heu g

priorityMoves :: (Game -> Int) -> Game -> MinPQueue Int Game
priorityMoves heu game = PQ.fromList $ zip (Prelude.map heu moves) moves
                where moves = concatMap ($ game) [move1M, move2M, move1C, move1M1C, move2C]