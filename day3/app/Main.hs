{-# LANGUAGE NamedFieldPuns #-}
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

data Symbol = Symbol Int Int Char
data Number = Number Int (Int, Int) Int

computeProblem1Solution :: [String] -> Int
computeProblem1Solution allLines =
  let (nums, syms) = parseLines allLines
  in sum $ map (\(Number _ _ val) -> val) $ filter (isPartNumber syms) nums

parseLines :: [String] -> ([Number], [Symbol])
parseLines ss =
  let (_, acc) = foldl (\(lineNb, (nums, syms)) line -> let (newNums, newSyms) = parseLine lineNb line in (lineNb + 1, (nums ++ newNums, syms ++ newSyms)))
                       (0, ([], []))
                       ss
  in acc

parseLine :: Int -> String -> ([Number], [Symbol])
parseLine lineNb s = go 0 s ([], [])
  where go _ [] acc = acc
        go colNb s (nums, syms) = case span isDigit s of
          ([], sym:rest) -> case span (== '.') s of
                               ([], _) -> go (colNb + 1) rest $ (nums, (Symbol lineNb colNb sym):syms)
                               (dots, remaining) -> go (colNb + (length dots)) remaining (nums, syms)
          (digits, remaining) -> go (colNb + (length digits)) remaining $ ((Number lineNb (colNb, colNb + (length digits) - 1) (read digits)):nums, syms)

isPartNumber :: [Symbol] -> Number -> Bool
isPartNumber syms (Number lineNb (colStart, colEnd) _) = any (isAdjacent lineNb colStart colEnd) syms

isAdjacent :: Int -> Int -> Int -> Symbol -> Bool
isAdjacent lineNb colStart colEnd (Symbol l c _) = l >= lineNb - 1 && l <= lineNb + 1 && c >= colStart - 1 && c <= colEnd + 1

computeProblem2Solution :: [String] -> Int
computeProblem2Solution allLines =
  let (nums, syms) = parseLines allLines
      partNumbers = filter (isPartNumber syms) nums
  in sum $ map (gearRatio partNumbers) syms

gearRatio :: [Number] -> Symbol -> Int
gearRatio partsNumber s@(Symbol _ _ '*') =
  case filter (\(Number lineNb (colStart, colEnd) _) -> isAdjacent lineNb colStart colEnd s) partsNumber of
    ((Number _ _ val1):(Number _ _ val2):[]) -> val1 * val2
    otherwise -> 0
gearRatio _ (Symbol _ _ _) = 0
