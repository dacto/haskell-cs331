module Main where

import System.Environment (getArgs)
import Data.List (transpose, genericLength, sort, partition)

type Probability = Double
type BayesTable  = (Probability, Probability)
type NaiveBayes  = [BayesTable]
data Review      = Negative | Positive deriving (Show, Eq)

main = do
	args <- getArgs
	trainData   <- readFile $ args !! 0
	trainLabels <- readFile $ args !! 1
	testData    <- readFile $ args !! 2
	testLabels  <- readFile $ args !! 3
	let bayesTables = map (calcTable $ getLabels trainLabels) . transpose . unCSV
	let classify    = bayes_classification $ bayesTables trainData
	let results     = zip (getLabels testLabels) $ map classify $ unCSV testData
	let (pos, neg)  = partition ((==Positive) . fst) results
	let posAcc      = (numReviews Positive $ map snd pos) * 100 `div` length pos
	let negAcc      = (numReviews Negative $ map snd neg) * 100 `div` length neg
	mapM_ putStrLn ["Positive: " ++ show posAcc ++ "%", "Negative: " ++ show negAcc ++ "%"]

{-
 - The classification function. Takes in a list of probability
 - tables and a list of Bools. Produces a review.
 -}
bayes_classification :: NaiveBayes -> [Bool] -> Review
bayes_classification a b = if (calcProb 0 a b) >= 0 then Positive else Negative

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
calcProb p ((a,b):xs) (y:ys)
	| y         = calcProb (p + log a       - log b)       xs ys
	| otherwise = calcProb (p + log (1 - a) - log (1 - b)) xs ys

{-
 - I save the probabilities for a positive or negative review
 - given the presence of the specific word. I also add in the
 - prior of 1/2.
 -}
calcTable :: [Review] -> [Bool] -> BayesTable
calcTable b a = (topA / bottomA, topB / bottomB)
	where
		topA    = fromIntegral (trueTrue a b 0) + 1
		bottomA = fromIntegral (numReviews Positive b) + 2
		topB    = fromIntegral (trueFalse a b 0) + 1
		bottomB = fromIntegral (numReviews Negative b) + 2

numReviews :: Review -> [Review] -> Int
numReviews = numReviews' 0

numReviews' :: Int -> Review -> [Review] -> Int
numReviews' c review [] = c
numReviews' c review (r:rs)
	| r == review = numReviews' (c + 1) review rs
	| otherwise   = numReviews' c review rs

trueTrue :: [Bool] -> [Review] -> Int -> Int
trueTrue [] _ c = c
trueTrue _ [] c = c
trueTrue (x:xs) (y:ys) c
	| not x || y == Negative = trueTrue xs ys c
	| otherwise              = trueTrue xs ys c + 1

trueFalse :: [Bool] -> [Review] -> Int -> Int
trueFalse [] _ c = c
trueFalse _ [] c = c
trueFalse (x:xs) (y:ys) c
	| not x || y == Positive = trueFalse xs ys c
	| otherwise              = trueFalse xs ys c + 1

unCSV :: String -> [[Bool]]
unCSV = map parseNums . tail . lines

getLabels :: String -> [Review]
getLabels = parseLabels . lines

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

parseLabels :: [String] -> [Review]
parseLabels [] = []
parseLabels (x:xs)
	| x == "neg" = Negative : parseLabels xs
	| x == "pos" = Positive : parseLabels xs
	| otherwise  = parseLabels xs
