module Main where
import System.Environment
import Data.List
import Data.Map (Map, keys, adjust, fromList, toList, fromListWith, size, differenceWith, update, elems)
import qualified Data.Map as Map
import Control.Parallel.Strategies
import Control.DeepSeq

main = do
	rawList   <- readFile "raw.vocabulary.txt"
	stopList  <- readFile "stoplist.txt"
	trainData <- readFile "raw.train.txt"
	testData  <- readFile "raw.test.txt"
	let rawwr = fromList $ zip (words rawList) $ repeat 0
	let stops = fromList $ zip (words stopList) $ repeat 0 -- Make Map of stopWords with values to be 0
	let train = lines trainData
	let fours = fromListWith (+) $ zip ((concat (map words train)) ++ words testData) $ repeat 1 -- tally up word occurrences
	let tempA = differenceWith ltf  rawwr stops -- tempB = rawList - stopWords
	let tempB = differenceWith ltf  fours stops -- tempA = (trainData&testData words > 4) - stopWords
	let clean = differenceWith ltf' tempA tempB -- clean = tempA - tempB
	let outfi = (unwords' $ keys clean):(map (vectorize clean) train) -- `using` parList rdeepseq)
	--writeFile "training.txt" unlines outfi
	putStrLn $ show $ size clean

ltf :: Int -> Int -> Maybe Int
ltf _ b | b > 3 = Just b
		| otherwise = Nothing

ltf' :: Int -> Int -> Maybe Int
ltf' _ b | b > 3 = Just 0
		| otherwise = Nothing

unwords' :: [String] -> String
unwords' [] =  ""
unwords' ws =  foldr1 (\w s -> w ++ ',':s) ws

vectorize :: Map String Int -> String -> String
vectorize a b = vectorize' a $ words b

vectorize' :: Map String Int -> [String] -> String
vectorize' clean [] = unwords' $ map show $ elems clean
vectorize' clean (x:xs) = vectorize' (update (\_-> Just 1) x clean) xs