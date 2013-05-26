module Main where

import System.Environment
import Data.List
import Data.Map (Map, keys, adjust, fromList)
import qualified Data.Map as Map

type StringCount = [(String, Int)]

main = do
	f1 <- readFile "raw.vocabulary.txt"
	f2 <- readFile "stoplist.txt"
	f3 <- readFile "raw.train.txt"
	let m = fromList $ zip (words f1 \\ words f2) $ repeat 0
	let l = keys $ Map.filter (>3) $ foldl' addToKey m $ words f3
	let w = intercalate "," l
	let n1 = map (makeVectorList l . sort . words) $ lines f3
	let n2 = intercalate "\n" $ w : (map (intercalate "," . (map show)) n1)
	writeFile "training.txt" n2

addToKey :: Map String Int -> String -> Map String Int
addToKey = flip $ adjust (+1)

makeVectorList :: [String] -> [String] -> [Int]
makeVectorList []     _      = []
makeVectorList (x:xs) []     = 0 : makeVectorList xs []
makeVectorList (x:xs) (y:ys) | x > y     = makeVectorList (x:xs) ys
                             | x < y     = 0 : makeVectorList xs (y:ys)
                             | otherwise = 1 : makeVectorList xs ys
