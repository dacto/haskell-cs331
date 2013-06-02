module Main where

import System.Environment (getArgs)
import Data.List (transpose, genericLength, sort, partition)

type Probability = Double
type BayesTable  = (Probability, Probability)
type NaiveBayes  = [BayesTable]

main = do
	args <- getArgs
	trainData   <- readFile $ args !! 0
	trainLabels <- readFile $ args !! 1
	testData    <- readFile $ args !! 2
	testLabels  <- readFile $ args !! 3
	let unCSV = map parseNums . tail . lines
	let getLabels = parseLabels . lines
	let bayesTables = map (calcTable $ getLabels trainLabels) . transpose . unCSV
	let classify = bayes_classification $ bayesTables trainData
	let results = zip (getLabels testLabels) $ map classify $ unCSV testData
	let (pos, neg) = partition ((==True) . fst) results
	let posAcc = foldl (flip ((+) . fromEnum . snd)) 0 pos * 100 `div` length pos
	let negAcc = (length neg - foldl (flip ((+) . fromEnum . snd)) 0 neg) * 100 `div` length neg
	mapM_ putStrLn ["Positive: " ++ show posAcc ++ "%", "Negative: " ++ show negAcc ++ "%"]

bayes_classification :: NaiveBayes -> [Bool] -> Bool
bayes_classification a b = (calcProb 0 a b) > 0

{-
 - Rather than doing a maximum of the probability of a positive
 - review and the probability of a negative review, I add the
 - probability of a positive review and subtract the probability
 - of a negative review. This way, I can do one pass through the
 - data and simply check if the result is greater than zero.
 - This also relies on the prior being 1/2, which it is in this
 - case.
 -}
calcProb :: Probability -> NaiveBayes -> [Bool] -> Probability
calcProb p [] _ = p
calcProb p _ [] = p
calcProb p (x:xs) (y:ys)
	| y         = calcProb (p + log (fst x)     - log (snd x))     xs ys
	| otherwise = calcProb (p + log (1 - fst x) - log (1 - snd x)) xs ys

{-
 - I save the probabilities for a positive or negative review
 - given the presence of the specific word. I also add in the
 - prior of 1/2.
 -}
calcTable :: [Bool] -> [Bool] -> BayesTable
calcTable b a = (topA / bottomA, topB / bottomB)
	where
		topA    = fromIntegral (trueTrue a b 0) + 1
		bottomA = fromIntegral (foldl (flip ((+) . fromEnum)) 0 b) + 2
		topB    = fromIntegral (trueFalse a b 0) + 1
		bottomB = fromIntegral (length b - (foldl (flip ((+) . fromEnum)) 0 b)) + 2

trueTrue :: [Bool] -> [Bool] -> Int -> Int
trueTrue [] _ c = c
trueTrue _ [] c = c
trueTrue (x:xs) (y:ys) c
	| not x || not y = trueTrue xs ys c
	| otherwise      = trueTrue xs ys c + 1

trueFalse :: [Bool] -> [Bool] -> Int -> Int
trueFalse [] _ c = c
trueFalse _ [] c = c
trueFalse (x:xs) (y:ys) c
	| not x || y = trueFalse xs ys c
	| otherwise  = trueFalse xs ys c + 1

parseWords :: String -> [String]
parseWords [] = []
parseWords (x:xs)
	| x == ','  = parseWords xs
	| otherwise = w : parseWords s
		where (w, s) = break (==',') (x:xs)

parseNums :: String -> [Bool]
parseNums [] = []
parseNums (x:xs)
	| x == '0'  = False : parseNums xs
	| x == '1'  = True  : parseNums xs
	| otherwise = parseNums xs

parseLabels :: [String] -> [Bool]
parseLabels [] = []
parseLabels (x:xs)
	| x == "neg" = False : parseLabels xs
	| x == "pos" = True  : parseLabels xs
	| otherwise  = parseLabels xs
