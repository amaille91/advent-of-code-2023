module Main where

import System.Environment (getArgs)
import Debug.Trace (trace)
import Control.Monad (foldM, join)
import Control.Applicative ((<|>))
import Data.Array (Array, array, (!), elems, (//), bounds)
import qualified Data.Array (Array, array)
import Data.Graph (Graph, graphFromEdges', stronglyConnComp)

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

data TileType = Vert | Hor | NE | NW | SW | SE | Ground | Start deriving (Eq, Show)
type Tile = (Coord, TileType)
data Direction = N | S | E | W deriving Show
type Coord = (Int, Int)

charToTileType :: Char -> Maybe TileType
charToTileType '|' = Just Vert
charToTileType '-' = Just Hor
charToTileType 'L' = Just NE
charToTileType 'J' = Just NW
charToTileType '7' = Just SW
charToTileType 'F' = Just SE
charToTileType '.' = Just Ground
charToTileType 'S' = Just Start
charToTileType _ = Nothing

parseInput :: String -> Maybe (Coord, Array Coord Tile)
parseInput s = do
  (_, maybePos, listOfTiles) <- foldM (\(lineNb, startPos, acc) line -> do
                                          (maybeStartPos, parsedLine) <- parseLine lineNb line
                                          return (lineNb + 1, startPos <|> maybeStartPos, acc <> parsedLine))
                                       (0, Nothing, [])
                                       (lines s)
  pos <- maybePos
  let coords = map fst listOfTiles
  return (pos, array (minimum coords, maximum coords) $ map (\(c, tileType) -> (c, (c, tileType))) listOfTiles)

parseLine :: Int -> String -> Maybe (Maybe Coord, [Tile])
parseLine lineNb line = do
  (_, arr, maybeStartPos) <- foldM (\(colNb, acc, startPos) c -> do
                                       tileType <- charToTileType c
                                       return (colNb + 1, ((lineNb, colNb), tileType):acc, if tileType == Start then Just (lineNb, colNb) else startPos))
                                   (0, [], Nothing)
                                   line
  return (maybeStartPos, arr)

possibleStartingPipes :: Coord -> Array Coord Tile -> [(Array Coord Tile, [(Tile, Direction)])]
possibleStartingPipes startPos arr =
  filter ((== 2) . length . snd) $ map (\tiletype -> (arr // [(startPos, (startPos, tiletype))], possibleMoves arr (startPos, tiletype))) [Vert, Hor, NE, NW, SE, SW]

possibleMoves :: Array Coord Tile -> Tile -> [(Tile, Direction)]
possibleMoves arr currentTile@(pos, tiletype) =
  filter (isMoveConnected tiletype) $
  map (\(coord, dir) -> (arr ! coord, dir)) $
  filter (isMoveInBounds (bounds arr)) $ naivePossibleMoves currentTile
  where
    isMoveInBounds bounds (coord, _) = isInBounds bounds coord
    isMoveConnected from ((_, to), dir) = areTilesConnected from to dir

naivePossibleMoves :: Tile -> [(Coord, Direction)]
naivePossibleMoves ((l, c), Vert) = [((l - 1, c), N), ((l + 1, c), S)]
naivePossibleMoves ((l, c), Hor) = [((l, c - 1), W), ((l, c + 1), E)]
naivePossibleMoves ((l, c), NE) = [((l - 1, c), N), ((l, c + 1), E)]
naivePossibleMoves ((l, c), NW) = [((l - 1, c), N), ((l, c - 1), W)]
naivePossibleMoves ((l, c), SW) = [((l + 1, c), S), ((l, c - 1), W)]
naivePossibleMoves ((l, c), SE) = [((l + 1, c), S), ((l, c - 1), W)]
naivePossibleMoves _ = []

isInBounds :: (Coord, Coord) -> Coord -> Bool
isInBounds ((minLine, minCol), (maxLine, maxCol)) (l, c) = l >= minLine && l <= maxLine && c >= minCol && c <= maxCol

areTilesConnected :: TileType -> TileType -> Direction -> Bool
areTilesConnected NE NW E = True
areTilesConnected NE SW E = True
areTilesConnected NE Hor E = True
areTilesConnected NE _ E = False
areTilesConnected NE SE N = True
areTilesConnected NE SW N = True
areTilesConnected NE Vert N = True
areTilesConnected NE _ N = False
areTilesConnected NW SE W = True
areTilesConnected NW NE W = True
areTilesConnected NW Hor W = True
areTilesConnected NW _ W = False
areTilesConnected NW SE N = True
areTilesConnected NW SW N = True
areTilesConnected NW Vert N = True
areTilesConnected NW _ N = False

areTilesConnected SE NW E = True
areTilesConnected SE SW E = True
areTilesConnected SE Hor E = True
areTilesConnected SE _ E = False
areTilesConnected SE NE S = True
areTilesConnected SE NW S = True
areTilesConnected SE Vert S = True
areTilesConnected SE _ S = False
areTilesConnected SW SE W = True
areTilesConnected SW NE W = True
areTilesConnected SW Hor W = True
areTilesConnected SW _ W = False
areTilesConnected SW NE S = True
areTilesConnected SW NW S = True
areTilesConnected SW Vert S = True
areTilesConnected SW _ S = False

areTilesConnected Hor NW E = True
areTilesConnected Hor SW E = True
areTilesConnected Hor Hor E = True
areTilesConnected Hor _ E = False
areTilesConnected Hor NE W = True
areTilesConnected Hor SE W = True
areTilesConnected Hor Hor W = True
areTilesConnected Hor _ W = False
areTilesConnected Vert SE N = True
areTilesConnected Vert SW N = True
areTilesConnected Vert Vert N = True
areTilesConnected Vert _ N = False
areTilesConnected Vert NE S = True
areTilesConnected Vert NW S = True
areTilesConnected Vert Vert S = True
areTilesConnected Vert _ S = False

computeProblem1Solution :: String -> Maybe Int
computeProblem1Solution s = do
  (startPos, arr) <- parseInput s
  trace (show (possibleStartingPipes startPos arr)) (return ())
  return (fst startPos)

computeProblem2Solution :: String -> Maybe Int
computeProblem2Solution s = return 0
