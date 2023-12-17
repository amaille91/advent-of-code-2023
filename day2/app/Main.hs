{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Char (isDigit)
import Data.Foldable (foldl)
import Data.Text (Text, pack)
import Data.List (groupBy, sortBy)
import Control.Monad (join)
import Text.Parsec (Parsec, many, many1, optional, (<|>), parse)
import Text.Parsec.Char (string, digit, char)
import Text.Parsec.Error (ParseError)
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

computeProblemSolution :: Int -> [String] -> Either ParseError Int
computeProblemSolution 1 input = computeProblem1Solution input
computeProblemSolution 2 input = computeProblem2Solution input

data Game = Game {ident :: Int, draws :: [Draw]}
type Draw = [CubeSet]
data CubeSet = CubeSet { number :: Int, color :: Color } deriving Show
data Color = R | G | B deriving (Eq, Show, Ord)

toColor :: String -> Color
toColor "red" = R
toColor "blue" = B
toColor "green" = G

inputParser :: Parsec Text () Game
inputParser = do
  gameId <- gameIdParser
  drawList <- many drawParser
  return $ Game { ident = gameId, draws = drawList }

gameIdParser :: Parsec Text () Int
gameIdParser = do
  string "Game "
  id <- read <$> many digit
  string ": "
  return id

drawParser :: Parsec Text () Draw
drawParser = many1 cubeSet
  where
    cubeSet :: Parsec Text () CubeSet
    cubeSet = do
      number <- read <$> many digit
      char ' '
      color <- toColor <$> (string "blue" <|> string "red" <|> string "green")
      optional $ string ", " <|> string "; "
      return $ CubeSet { number = number, color = color }

computeProblem1Solution :: [String] -> Either ParseError Int
computeProblem1Solution allLines = do
  games <- sequence $ parseLines allLines
  return $ sum $ map ident $ filter isGamePossible $ games

parseLines :: [String] -> [Either ParseError Game]
parseLines = map ((parse inputParser "input") . pack)

isGamePossible :: Game -> Bool
isGamePossible (Game { ident, draws }) =
  case draws of
    [] -> True
    draw:rest -> isDrawPossible draw && isGamePossible (Game { ident = ident, draws = rest })
  where
    isDrawPossible :: Draw -> Bool
    isDrawPossible cubeSets = all isPossibleCubeSet cubeSets
    isPossibleCubeSet :: CubeSet -> Bool
    isPossibleCubeSet (CubeSet { number, color }) = case color of
      R -> number <= 12
      G -> number <= 13
      B -> number <= 14

computeProblem2Solution :: [String] -> Either ParseError Int
computeProblem2Solution allLines = do
  games <- sequence $ parseLines allLines
  return $ sum $ map minimumSetPower games

minimumSetPower :: Game -> Int
minimumSetPower (Game { draws }) = product $ minimumSet draws
  where minimumSet :: [Draw] -> [Int]
        minimumSet ds = map (maximum . (map number)) $ groupBy (\c1 c2 -> color c1 == color c2) $ sortBy (\c1 c2 -> compare (color c1) (color c2)) $ join draws
