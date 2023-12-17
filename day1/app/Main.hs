module Main where

import Data.Char (isDigit)
import Data.Foldable (foldl)
import Debug.Trace (trace)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then print "The program needs a '--part' flag to indicate which problem to solve"
  else do
    allLines <- lines <$> readFile "input.txt"
    print $ computeProblemSolution (read $ args !! 1) allLines

computeProblemSolution :: Int -> [String] -> Int
computeProblemSolution 1 input = computeProblem1Solution input
computeProblemSolution 2 input = computeProblem2Solution input

computeProblem1Solution :: [String] -> Int
computeProblem1Solution allLines = sum $ map computeCalibrationValue allLines
  where 
    computeCalibrationValue :: String -> Int
    computeCalibrationValue s =
      let digits@(firstDigit:_) = filter isDigit s
          lastDigit       = head $ reverse digits
      in read (firstDigit:lastDigit:[])

computeProblem2Solution :: [String] -> Int
computeProblem2Solution s = sum $ map computeComplexCalibrationValue s

computeComplexCalibrationValue :: String -> Int
computeComplexCalibrationValue s = read $ [firstDigit, lastDigit]
  where (Just firstDigit, rest) = computeNextDigit s
        lastDigit     = computeLastDigit firstDigit rest
        computeLastDigit lastDigitSeen s = case computeNextDigit s of
          (Just d, rem) -> computeLastDigit d rem
          (Nothing, _) -> lastDigitSeen
        computeNextDigit [] = (Nothing, [])
        computeNextDigit ('1':r) = (Just '1', r)
        computeNextDigit ('2':r) = (Just '2', r)
        computeNextDigit ('3':r) = (Just '3', r)
        computeNextDigit ('4':r) = (Just '4', r)
        computeNextDigit ('5':r) = (Just '5', r)
        computeNextDigit ('6':r) = (Just '6', r)
        computeNextDigit ('7':r) = (Just '7', r)
        computeNextDigit ('8':r) = (Just '8', r)
        computeNextDigit ('9':r) = (Just '9', r)
        computeNextDigit ('o':'n':'e':r) = (Just '1', r)
        computeNextDigit ('t':'w':'o':r) = (Just '2', r)
        computeNextDigit ('t':'h':'r':'e':'e':r) = (Just '3', r)
        computeNextDigit ('f':'o':'u':'r':r) = (Just '4', r)
        computeNextDigit ('f':'i':'v':'e':r) = (Just '5', r)
        computeNextDigit ('s':'i':'x':r) = (Just '6', r)
        computeNextDigit ('s':'e':'v':'e':'n':r) = (Just '7', r)
        computeNextDigit ('e':'i':'g':'h':'t':r) = (Just '8', r)
        computeNextDigit ('n':'i':'n':'e':r) = (Just '9', r)
        computeNextDigit (_:r) = computeNextDigit r
