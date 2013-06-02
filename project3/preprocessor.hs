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
	f4 <- readFile "raw.test.txt"
	let wordMap = (\x y -> fromList $ zip (words x \\ words y) (repeat 0))
	let headers = keys $ Map.filter (>3) $ foldl' addToKey (wordMap f1 f2) $ words f3 ++ words f4
	let addCommas = intercalate ","
	let addNewlines = intercalate "\n"
	let csv'ify = addNewlines . ((addCommas headers) :) . map (addCommas . (map show))
	let vectorize_file = csv'ify . map (makeVectorList headers . sort . words) . lines
	mapM_ (\(x,y) -> writeFile x $ vectorize_file y) [("training.txt", f3), ("test.txt", f4)]

addToKey :: Map String Int -> String -> Map String Int
addToKey = flip $ adjust (+1)

makeVectorList :: [String] -> [String] -> [Int]
makeVectorList []     _      = []
makeVectorList (x:xs) []     = 0 : makeVectorList xs []
makeVectorList (x:xs) (y:ys) | x > y     = makeVectorList (x:xs) ys
                             | x < y     = 0 : makeVectorList xs (y:ys)
                             | otherwise = 1 : makeVectorList xs ys
