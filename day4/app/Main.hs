{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Char (isDigit)
import Data.Set (Set, fromList, intersection)
import qualified Data.Map as Map
import Data.List (words)
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

computeProblemSolution :: Int -> [String] -> Maybe Int
computeProblemSolution 1 input = computeProblem1Solution input
computeProblemSolution 2 input = computeProblem2Solution input

data Card = Card (Set Int) (Set Int) Int

computeProblem1Solution :: [String] -> Maybe Int
computeProblem1Solution allLines =
  let cards = parseLines allLines
  in Just $ sum $ map computePoints cards

parseLines :: [String] -> [Card]
parseLines ss = map parseLine ss
  where
    parseLine :: String -> Card
    parseLine s = let (winning, _:have) = span (/= "|") $ drop 2 $ words s
      in Card (fromList (map read winning)) (fromList (map read have)) 1

computePoints (Card winning have _) = if ((length inter) /= 0) then 2 ^ ((length inter) - 1) else 0
  where inter = intersection winning have

computeProblem2Solution :: [String] -> Maybe Int
computeProblem2Solution allLines =
  let cards = (parseLines allLines)
      mapOfCards = Map.fromList $ zip [1..] cards
  in Map.foldl (\acc (Card _ _ n) -> acc + n) 0 <$> foldl processCard (Just mapOfCards) [1..(length cards)]

processCard :: Maybe (Map.Map Int Card) -> Int -> Maybe (Map.Map Int Card)
processCard maybeMap i = do
  m <- maybeMap
  (Card winning have nbCards) <- Map.lookup i m
  let nbMatching = length $ intersection winning have
  return $ foldl (addCards nbCards) m [i+1..i + nbMatching]
  where
    addCards :: Int -> Map.Map Int Card -> Int -> Map.Map Int Card
    addCards nbCards m k = Map.adjust (\(Card w h nb) -> Card w h (nb + nbCards)) k m
