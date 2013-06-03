module Main where
import System.Environment
import Data.List

usage = putStrLn "Usage: bayes < training data file > < training label file > \
				 \< testing data file > < testing label file >"

type Probability = Double
type Table  = (Probability, Probability)

main = do
	args <- getArgs
	case args of
		[]     -> usage
		["-h"] -> usage
		(trDFn:trLFn:teDFn:teLFn:[]) -> do
			trD <- readFile trDFn
			trL <- readFile trLFn
			teD <- readFile teDFn
			teL <- readFile teLFn
			let uncsvdata   = map (dataToNum []) . tail . lines
			let classlabels = (lblToNum []) . lines
			let table 		= map (pWord $ classlabels trL) . transpose . uncsvdata
			let classify 	= zip (classlabels teL) $ map (classification $ table trD) $ uncsvdata teD
			let (pC, pI) 	= prediction classify ((0,0), (0,0))
			mapM_ putStrLn ["Positive: " ++ show pC ++ "%", "Negative: " ++ show pI ++ "%"]
		_ -> usage

prediction :: [(Int, Int)] -> ((Int,Int), (Int,Int)) -> (Int, Int)
prediction [] ((p,q),(y,z)) = ((p*100) `div` q, (y*100) `div` z)
prediction ((x,w):xs) ((p,q),(y,z)) | x == 1 = prediction xs ((p+(tupleMatch 1 w), q+1),(y,z))
									| x == 0 = prediction xs ((p,q),(y+(tupleMatch 0 w),z+1))

tupleMatch :: Int -> Int -> Int
tupleMatch c a 	| c == a 	= 1
				| otherwise = 0

pWord :: [Int] -> [Int] -> Table
pWord cl w = (numA/denA, numB/denB)
		where 
			numA = fromIntegral (oo cl w 0) + 1
			numB = fromIntegral (zo cl w 0) + 1
			denA = fromIntegral (sum cl) + 2
			denB = fromIntegral (length cl - sum cl) +2

-- oo | True True -> Count
oo :: [Int] -> [Int] -> Int -> Int
oo [] _ i = i
oo _ [] i = i
oo (x:xs) (y:ys) i | (x+y) == 2 = oo xs ys $ i+1
				   | otherwise  = oo xs ys i


-- zo | False True -> Count
zo :: [Int] -> [Int] -> Int -> Int
zo [] _ i = i
zo _ [] i = i
zo (x:xs) (y:ys) i | (y-x) == 1 = zo xs ys $ i+1
				   | otherwise  = zo xs ys i

classification :: [Table] -> [Int] -> Int
classification a b | p > 0 = 1
				   | otherwise = 0
				   where p = calcProb 0 a b

calcProb :: Probability -> [Table] -> [Int] -> Probability
calcProb p [] _ = p
calcProb p _ [] = p
calcProb p ((x,w):xs) (y:ys)
			| (y == 1)  = calcProb (p + log x - log w) xs ys
			| otherwise = calcProb (p + log (1 - x) - log (1 - w)) xs ys

dataToNum :: [Int] -> String -> [Int]
dataToNum y [] = y
dataToNum y (x:xs) = case x of
					'0' -> dataToNum (0:y) xs
					'1' -> dataToNum (1:y) xs
					_   -> dataToNum y xs

lblToNum :: [Int] -> [String] -> [Int]
lblToNum y [] = y
lblToNum y (x:xs) = case x of
					"neg" -> lblToNum (0:y) xs
					"pos" -> lblToNum (1:y) xs
					_ 	  -> lblToNum y xs
