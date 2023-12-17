module Main where

import System.Environment (getArgs)
import Prelude hiding (lookup)
import Data.Map (Map, fromList)
import Data.Map.Strict (lookup, foldlWithKey)
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

data Instruction = L | R deriving Show
instructionFromChar :: Char -> Maybe Instruction
instructionFromChar 'L' = Just L
instructionFromChar 'R' = Just R
instructionFromChar _ = Nothing

parseInput :: String -> Maybe ([Instruction], Map String (String, String))
parseInput s = case lines s of
  (l1:l2:rest) -> do
    instructions <- sequence $ map instructionFromChar l1
    return (instructions, fromList $ map toListEntry rest)
  _ -> Nothing

toListEntry :: String -> (String, (String, String))
toListEntry s = let (key, rest) = span (/= ' ') s
                    leftSol = take 3 $ drop 4 rest
                    rightSol = take 3 $ drop 9 rest
                in (key, (leftSol, rightSol))

computeProblem1Solution :: String -> Maybe Int
computeProblem1Solution input = do
  (instructions, m) <- parseInput input
  go (cycle instructions) m "AAA" 0
  where
    go [] _ _ _ = Nothing
    go _ _ "ZZZ" acc = return acc
    go (L:rest) m node acc = do
      (leftNode, _) <- lookup node m
      go rest m leftNode (acc + 1)
    go (R:rest) m node acc = do
      (_, rightNode) <- lookup node m
      go rest m rightNode (acc + 1)

findCycleLength :: [Instruction] -> Map String (String, String) -> String -> Maybe Int
findCycleLength inst m startingNode = go (cycle inst) m startingNode 0
  where go [] _ _ _ = Nothing
        go _ _ [_, _, 'Z'] acc = return acc
        go (L:rest) m node acc = do
          (leftNode, _) <- lookup node m
          go rest m leftNode (acc + 1)
        go (R:rest) m node acc = do
          (_, rightNode) <- lookup node m
          go rest m rightNode (acc + 1)

computeProblem2Solution :: String -> Maybe Int
computeProblem2Solution input = do
  (instructions, m) <- parseInput input
  let
    startingNodes :: [String]
    startingNodes = foldlWithKey (\acc k _ -> if last k == 'A' then k:acc else acc) [] m
  foldl lcm 1 <$> (sequence $ map (findCycleLength instructions m) startingNodes)
