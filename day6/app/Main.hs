module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)

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

computeProblem1Solution :: String -> Maybe Int
computeProblem1Solution input = do
  races <- parseInput input
  return $ product $ map nbRecordRaces races

parseInput :: String -> Maybe [(Int, Int)]
parseInput s = case lines s of
  (l1:l2:[]) -> Just $ zip (parseLine l1) (parseLine l2)
  _          -> Nothing

parseLine :: String -> [Int]
parseLine [] = []
parseLine s = let (digits, rest) = span isDigit $ dropWhile (not . isDigit) s
              in (read digits):parseLine rest

nbRecordRaces :: (Int, Int) -> Int
nbRecordRaces (time, record) = let recordBrokenOnHalfTimes = 2 * (length $ filter (> record) $ map (score time) [0 .. time `div` 2])
                               in if (time `rem` 2 == 0) then recordBrokenOnHalfTimes - 1 else recordBrokenOnHalfTimes

score :: Int -> Int -> Int
score maxTime pushTime = pushTime * (maxTime - pushTime)

computeProblem2Solution :: String -> Maybe Int
computeProblem2Solution allLines = do
  (time, record) <- parseLines2 allLines
  let firstTimeWinning = findFirstTimeWinning (\pushTime -> record < score time pushTime) 0 (time `div` 2)
  return $ 2 * ((time `div` 2) - firstTimeWinning + 1) - (if (time `rem` 2 == 0) then 1 else 0)

parseLines2 :: String -> Maybe (Int, Int)
parseLines2 s = case lines s of
  (l1:l2:[]) -> Just (read $ filter isDigit l1, read $ filter isDigit l2)
  _          -> Nothing

findFirstTimeWinning :: (Int -> Bool) -> Int -> Int -> Int
findFirstTimeWinning isWinning lowBound highBound
  | highBound - lowBound <= 1 = highBound
  | isWinning (lowBound + ((highBound - lowBound) `div` 2)) = findFirstTimeWinning isWinning lowBound (lowBound + ((highBound - lowBound) `div` 2))
  | otherwise = findFirstTimeWinning isWinning (lowBound + ((highBound - lowBound) `div` 2)) highBound
