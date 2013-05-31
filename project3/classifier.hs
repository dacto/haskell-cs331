module Main where

import Data.List (transpose, genericLength)
import Data.Ratio ((%), numerator, denominator)
import Data.Ratio as R

main = do
	f1 <- readFile "training.txt"
	f2 <- readFile "train.label"
	let headers = ((++["review"]) . parseWords . head . lines) f1
	let col_data = (transpose . map parseNums . tail . lines) f1
	let col_results = (parseLabels . lines) f2
	let prior = sum col_results % genericLength col_results
	let bayes_tables = map (\x -> calcTable prior x col_results) col_data
	mapM_ (putStrLn . show) $ take 25 bayes_tables

calcTable :: Rational -> [Integer] -> [Integer] -> (Rational, Rational)
calcTable prior a b = (topA % bottomA, topB % bottomB)
	where
		topA = (trueTrue a b 0) + numerator prior
		bottomA = (sum b) + denominator prior
		topB = (trueFalse a b 0) + numerator prior
		bottomB = (genericLength b - sum b) + denominator prior

trueTrue [] _ c = c
trueTrue _ [] c = c
trueTrue (x:xs) (y:ys) c
	| x == 0 || y == 0 = trueTrue xs ys c
	| otherwise        = trueTrue xs ys c+1

trueFalse [] _ c = c
trueFalse _ [] c = c
trueFalse (x:xs) (y:ys) c
	| x == 0 || y == 1 = trueFalse xs ys c
	| otherwise        = trueFalse xs ys c+1

parseWords :: String -> [String]
parseWords [] = []
parseWords (x:xs)
	| x == ','  = parseWords xs
	| otherwise = w : parseWords s
		where (w, s) = break (==',') (x:xs)

parseNums :: String -> [Integer]
parseNums [] = []
parseNums (x:xs)
	| x == '0'  = 0 : parseNums xs
	| x == '1'  = 1 : parseNums xs
	| otherwise = parseNums xs

parseLabels :: [String] -> [Integer]
parseLabels [] = []
parseLabels (x:xs)
	| x == "neg" = 0 : parseLabels xs
	| x == "pos" = 1 : parseLabels xs
	| otherwise  = parseLabels xs
