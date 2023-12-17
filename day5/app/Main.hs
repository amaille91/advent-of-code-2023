module Main where

import System.Environment (getArgs)
import Data.List (sortOn)
import Text.Parsec (Parsec, ParseError, parse, many1, manyTill, eof, (<|>), try, many)
import Text.Parsec.Char (string, digit, space, newline, anyChar)
import Data.Text (Text, pack)
import Debug.Trace (trace)

main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 2
     then print "Which part should we play ? Please use the --part flag"
     else computeSolution (read $ (args !! 1)) <$> lines <$> readFile "input.txt" >>= print

computeSolution :: Int -> [String] -> Either ParseError Int
computeSolution 1 ss = computeSolution1 ss
computeSolution 2 ss = computeSolution2 ss
computeSolution _ _ = return $ -1

type Seed = Int
type Map = [((Int, Int), (Int, Int), Int)]

parseLines :: [String] -> Either ParseError ([Seed], [Map])
parseLines allLines = parse problemParser "input" (pack $ unlines allLines) where
  problemParser :: Parsec Text () ([Seed], [Map])
  problemParser = do
    _ <- string "seeds: "
    seeds <- (map read) <$> manyTill ((many1 digit) >>= (\digits -> space >> return digits)) newline
    _ <- manyTill anyChar newline
    seedToSoilMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    soilToFertMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    fertToWaterMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    waterToLightMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    lightToTempMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    tempToHumidityMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    humidityToLocationMap <- manyTill parseRange eof
    return (seeds, [humidityToLocationMap, tempToHumidityMap, lightToTempMap, waterToLightMap, fertToWaterMap, soilToFertMap, seedToSoilMap])

parseRange :: Parsec Text () ((Int, Int), (Int, Int), Int)
parseRange = do
  outStart <- read <$> manyTill digit space
  inStart <- read <$> manyTill digit space
  _length <- read <$> manyTill digit newline
  return ((inStart, inStart + _length - 1), (outStart, outStart + _length - 1), outStart - inStart)

sortIn :: Map -> Map
sortIn m = sortOn (\((inStart, _), _, _) -> inStart) m

sortOut :: Map -> Map
sortOut m = sortOn (\(_, (outStart, _), _) -> outStart) m

computeSolution1 :: [String] -> Either ParseError Int
computeSolution1 ss = do
  (seeds, maps) <- parseLines ss
  let finalMap = reduceMaps maps
  return $ minimum $ map (projectSeed finalMap) seeds

projectSeed :: Map -> Seed -> Int
projectSeed [] seed = seed
projectSeed m@(((inStart, inEnd), _, offset):rest) seed = if seed >= inStart && seed <= inEnd then seed + offset else projectSeed rest seed

data ScanningState = None | In | Out | Both

reduceMaps :: [Map] -> Map
reduceMaps [] = []
reduceMaps (startingMap:rest) = foldl (mergeMaps None) startingMap rest

mergeMaps :: ScanningState -> Map -> Map -> Map
mergeMaps state mOut mIn = go state (sortIn mOut) (sortOut mIn)

go :: ScanningState -> Map -> Map -> Map
go _ [] [] = []
go _ mOut [] = mOut
go _ [] mIn = mIn
go None mOut@(((outMapInStart, _), _, _):_)
               mIn@((_, (inMapOutStart, _), _):_)
  | inMapOutStart == outMapInStart = mergeMaps Both mOut mIn
  | inMapOutStart < outMapInStart = mergeMaps In mOut mIn
  | otherwise = mergeMaps Out mOut mIn
go In mOut@(((outMapInStart, outMapInEnd), (outMapOutStart, outMapOutEnd), offset2):restOut)
             (rIn@((inMapInStart, inMapInEnd), (inMapOutStart, inMapOutEnd), offset1):restIn)
  | inMapOutEnd < outMapInStart = rIn:mergeMaps None mOut restIn
  | otherwise = let subRangeLength = outMapInStart - inMapOutStart
                    firstMap = ((inMapInStart, inMapInStart + subRangeLength - 1), (inMapOutStart, inMapOutStart + subRangeLength - 1), offset1)
                    remainingInMap = ((inMapInStart + subRangeLength, inMapInEnd), (inMapOutStart + subRangeLength, inMapOutEnd), offset1)
                in
                    firstMap:(mergeMaps Both mOut (remainingInMap:restIn))
go Out (rOut@((outMapInStart, outMapInEnd), (outMapOutStart, outMapOutEnd), offset2):restOut)
             mIn@(((inMapInStart, inMapInEnd), (inMapOutStart, inMapOutEnd), offset1):restIn)
  | outMapInEnd < inMapOutStart = rOut:mergeMaps None restOut mIn
  | otherwise = let subRangeLength = inMapOutStart - outMapInStart
                    firstMap = ((outMapInStart, outMapInStart + subRangeLength - 1), (outMapOutStart, outMapOutStart + subRangeLength - 1), offset2)
                    remainingOutMap = ((outMapInStart + subRangeLength, outMapInEnd), (outMapOutStart + subRangeLength, outMapOutEnd), offset2)
                in
                    firstMap:(mergeMaps Both (remainingOutMap:restOut) mIn)
go Both (((outMapInStart, outMapInEnd), (outMapOutStart, outMapOutEnd), offset2):restOut)
               (((inMapInStart, inMapInEnd),   (inMapOutStart, inMapOutEnd),   offset1):restIn)
  | inMapOutEnd == outMapInEnd = ((inMapInStart, inMapInEnd), (outMapOutStart, outMapOutEnd), offset1 + offset2):(mergeMaps None restOut restIn)
  | inMapOutEnd < outMapInEnd = let subRangeLength = inMapOutEnd - inMapOutStart + 1
                                    firstMap = ((inMapInStart, inMapInStart + subRangeLength - 1), (outMapOutStart, outMapOutStart + subRangeLength - 1), offset1 + offset2)
                                    remainingOutMap = ((outMapInStart + subRangeLength, outMapInEnd), (outMapOutStart + subRangeLength, outMapOutEnd), offset2)
                                in firstMap:(mergeMaps None (remainingOutMap:restOut) restIn)
  | otherwise = let subRangeLength = outMapInEnd - outMapInStart + 1
                    firstMap = ((inMapInStart, inMapInStart + subRangeLength - 1), (outMapOutStart, outMapOutStart + subRangeLength - 1), offset1 + offset2)
                    remainingInMap = ((inMapInStart + subRangeLength, inMapInEnd), (inMapOutStart + subRangeLength, inMapOutEnd), offset1)
                in firstMap:(mergeMaps None restOut (remainingInMap:restIn))


type Seeds2 = [(Int, Int)]

computeSolution2 :: [String] -> Either ParseError Int
computeSolution2 allLines = do
  (seeds, maps) <- parseLines2 allLines
  let reducedMap = reduceMaps maps
  return $ minimum $ map (\(_, (outStart, _), _) -> outStart) $ cropRanges seeds $ reducedMap

parseLines2 :: [String] -> Either ParseError (Seeds2, [Map])
parseLines2 allLines = parse problemParser "input" (pack $ unlines allLines) where
  problemParser :: Parsec Text () (Seeds2, [Map])
  problemParser = do
    _ <- string "seeds: "
    seeds <- parseSeeds
    _ <- manyTill anyChar newline
    seedToSoilMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    soilToFertMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    fertToWaterMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    waterToLightMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    lightToTempMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    tempToHumidityMap <- manyTill parseRange newline
    _ <- manyTill anyChar newline
    humidityToLocationMap <- manyTill parseRange eof
    return (seeds, [humidityToLocationMap, tempToHumidityMap, lightToTempMap, waterToLightMap, fertToWaterMap, soilToFertMap, seedToSoilMap])

parseSeeds :: Parsec Text () Seeds2
parseSeeds = manyTill parseSeedRange newline
  where
    parseSeedRange = do
      start <- read <$> manyTill digit (try (space <|> newline))
      rangeLength <- read <$> manyTill digit (try (space <|> newline))
      return (start, start + rangeLength - 1)

cropRanges :: Seeds2 -> Map -> Map
cropRanges seeds m = concatMap (shrink seeds) m
  where
    shrink :: Seeds2 -> ((Int, Int), (Int, Int), Int) -> [((Int, Int), (Int, Int), Int)]
    shrink seeds range = 
      let
        shrinkedRange :: [Maybe ((Int, Int), (Int, Int), Int)]
        shrinkedRange = map (shrinkRange range) seeds
      in foldl (\acc shrinked -> case shrinked of
                                   Just s -> s:acc
                                   Nothing -> acc) [] shrinkedRange
    shrinkRange :: ((Int, Int), (Int, Int), Int) -> (Int, Int) -> Maybe ((Int, Int), (Int, Int), Int)
    shrinkRange ((inStart, inEnd), _, offset) (seedStart, seedEnd) =
      let start = max inStart seedStart
          end = min inEnd seedEnd
      in if start > end then Nothing else Just $ ((start, end), (start + offset, end + offset), offset)
