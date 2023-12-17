module Main where

import System.Environment (getArgs)
import Debug.Trace (trace)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then print "The program needs a '--part' flag to indicate which problem to solve"
  else do
    input <- readFile "input.txt"
    print $ computeProblemSolution (read $ args !! 1) input

computeProblemSolution :: Int -> String -> Maybe Int
computeProblemSolution 1 input = computeProblem1Solution input
computeProblemSolution 2 input = computeProblem2Solution input

type Sequence = [Int]

computeProblem1Solution :: String -> Maybe Int
computeProblem1Solution = return . sum . map computePrediction . parseInput

parseInput :: String -> [Sequence]
parseInput s = map (map read . words) $ lines s

differenceSequence :: Sequence -> Sequence
differenceSequence [] = []
differenceSequence (h:rest) = fst $ foldl (\(acc, previous) i -> (acc <> [i - previous], i)) ([], h) rest

computePrediction :: Sequence -> Int
computePrediction s = sum $ map last $ takeWhile (any (/= 0)) $ iterate differenceSequence s

computeProblem2Solution :: String -> Maybe Int
computeProblem2Solution = return . sum . map computeBackwardPrediction . parseInput

computeBackwardPrediction :: Sequence -> Int
computeBackwardPrediction s = foldr (\i acc -> i - acc) 0 $ map head $ takeWhile (any (/= 0)) $ iterate differenceSequence s
