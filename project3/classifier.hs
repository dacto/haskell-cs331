module Main where

import Data.List (transpose)

type Prob = (Bool, Double)
type Parent = (String, Prob)
type Child = (String, Prob, Prob)

main = do
	f1 <- readFile "training.txt"
	f2 <- readFile "train.label"
	let (l1:l2) = lines f1
	let w = parseWords l1
	let ns = map parseNums l2
	let ns' = transpose ns
	let r = parseLabels $ lines f2
	let p = ("Positive", (True, fromIntegral (sum r) / fromIntegral (length r)))
	putStrLn $ show p

parseWords :: String -> [String]
parseWords [] = []
parseWords (x:xs)
	| x == ','  = parseWords xs
	| otherwise = w : parseWords s
		where (w, s) = break (==',') (x:xs)

parseNums :: String -> [Int]
parseNums [] = []
parseNums (x:xs)
	| x == '0'  = 0 : parseNums xs
	| x == '1'  = 1 : parseNums xs
	| otherwise = parseNums xs

parseLabels :: [String] -> [Int]
parseLabels [] = []
parseLabels (x:xs)
	| x == "neg" = 0 : parseLabels xs
	| x == "pos" = 1 : parseLabels xs
	| otherwise  = parseLabels xs
