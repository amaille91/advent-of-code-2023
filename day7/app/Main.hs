module Main where

import Data.List (nub, sortOn)
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

newtype Hand = Hand [Card] deriving (Eq, Show)
data Card = A | K | Q | J | T | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Eq, Show)
toInt :: Card -> Int
toInt Two = 2
toInt Three = 3
toInt Four = 4
toInt Five = 5
toInt Six = 6
toInt Seven = 7
toInt Eight = 8
toInt Nine = 9
toInt T = 10
toInt J = 11
toInt Q = 12
toInt K = 13
toInt A = 14

cardFromChar :: Char -> Maybe Card
cardFromChar '2' = Just Two
cardFromChar '3' = Just Three
cardFromChar '4' = Just Four
cardFromChar '5' = Just Five
cardFromChar '6' = Just Six
cardFromChar '7' = Just Seven
cardFromChar '8' = Just Eight
cardFromChar '9' = Just Nine
cardFromChar 'T' = Just T
cardFromChar 'J' = Just J
cardFromChar 'Q' = Just Q
cardFromChar 'K' = Just K
cardFromChar 'A' = Just A
cardFromChar _ = Nothing

instance Ord Card where
  compare c1 c2 = compare (toInt c1) (toInt c2)

data HandType = FiveOfAKind | FourOfAKind | Full | ThreeOfAKind | TwoPair | Pair | High deriving (Eq, Show)
handTypeToInt :: HandType -> Int
handTypeToInt High = 1
handTypeToInt Pair = 2
handTypeToInt TwoPair = 3
handTypeToInt ThreeOfAKind = 4
handTypeToInt Full = 5
handTypeToInt FourOfAKind = 6
handTypeToInt FiveOfAKind = 7

instance Ord HandType where
  compare ht1 ht2 = compare (handTypeToInt ht1) (handTypeToInt ht2)

instance Ord Hand where
  compare h1@(Hand cards1) h2@(Hand cards2) = case compare (handType h1) (handType h2) of
    EQ -> compare cards1 cards2
    res -> res

handType :: Hand -> HandType
handType (Hand cards) =
  let uniq = nub cards
      occurences = map (\u -> length $ filter (== u) cards) uniq
  in case maximum occurences of
    5 -> FiveOfAKind
    4 -> FourOfAKind
    3 -> if 2 `elem` occurences then Full else ThreeOfAKind
    2 -> if length (filter (== 2) occurences) == 2 then TwoPair else Pair
    _ -> High
           

computeProblem1Solution :: String -> Maybe Int
computeProblem1Solution input = do
  hands <- parseInput input
  let sortedHands = sortOn fst hands
  return $ fst $ foldl (\(acc, rank) (_, bid) -> (acc + rank * bid, rank + 1)) (0, 1) sortedHands

parseInput :: String -> Maybe [(Hand, Int)]
parseInput s = sequence $ map parseLine (lines s)

parseLine :: String -> Maybe (Hand, Int)
parseLine l = case words l of
               (cardsChar:bidStr:[]) -> do
                 cards <- sequence $ map cardFromChar cardsChar
                 return (Hand cards, read bidStr)
               _ -> Nothing

newtype NewHand = NewHand [NewCard] deriving (Eq, Show)
data NewCard = NewA | NewK | NewQ | NewJ | NewT | NewNine | NewEight | NewSeven | NewSix | NewFive | NewFour | NewThree | NewTwo deriving (Eq, Show)
toIntNewCard :: NewCard -> Int
toIntNewCard NewTwo = 2
toIntNewCard NewThree = 3
toIntNewCard NewFour = 4
toIntNewCard NewFive = 5
toIntNewCard NewSix = 6
toIntNewCard NewSeven = 7
toIntNewCard NewEight = 8
toIntNewCard NewNine = 9
toIntNewCard NewT = 10
toIntNewCard NewJ = 1
toIntNewCard NewQ = 12
toIntNewCard NewK = 13
toIntNewCard NewA = 14

instance Ord NewCard where
  compare c1 c2 = compare (toIntNewCard c1) (toIntNewCard c2)

instance Ord NewHand where
  compare h1@(NewHand cards1) h2@(NewHand cards2) = case compare (newHandType h1) (newHandType h2) of
    EQ -> compare cards1 cards2
    res -> res

newHandType :: NewHand -> HandType
newHandType (NewHand cards) =
  let uniq = nub $ filter (/= NewJ) cards
      occurences = map (\u -> length $ filter (== u) cards) uniq
      nbJoker = length $ filter (== NewJ) cards
  in if nbJoker == 5
     then case nbJoker of
            5 -> FiveOfAKind
            4 -> FourOfAKind
            3 -> ThreeOfAKind
            2 -> Pair
            _ -> High
     else case maximum occurences of
            5 -> FiveOfAKind
            4 -> if nbJoker >= 1 then FiveOfAKind else FourOfAKind
            3 -> if nbJoker >= 2
                 then FiveOfAKind
                 else if nbJoker >= 1
                      then FourOfAKind
                      else if 2 `elem` occurences
                           then Full
                           else ThreeOfAKind
            2 -> if nbJoker >= 3
                 then FiveOfAKind
                 else if nbJoker >= 2
                      then FourOfAKind
                      else if nbJoker >= 1
                           then if length (filter (== 2) occurences) == 2 then Full else ThreeOfAKind
                           else if length (filter (== 2) occurences) == 2 then TwoPair else Pair
            1 -> if nbJoker >= 4
                 then FiveOfAKind
                 else if nbJoker >= 3
                      then FourOfAKind
                      else if nbJoker >= 2
                           then ThreeOfAKind
                           else if nbJoker >= 1
                                then Pair
                                else High
            _ -> High
computeProblem2Solution :: String -> Maybe Int
computeProblem2Solution input = do
  hands <- parseInput2 input
  let sortedHands = sortOn fst hands
  return $ fst $ foldl (\(acc, rank) (_, bid) -> (acc + rank * bid, rank + 1)) (0, 1) sortedHands

parseInput2 :: String -> Maybe [(NewHand, Int)]
parseInput2 s = sequence $ map parseLine2 (lines s)

parseLine2 :: String -> Maybe (NewHand, Int)
parseLine2 l = case words l of
                (cardsChar:bidStr:[]) -> do
                  cards <- sequence $ map newCardFromChar cardsChar
                  return (NewHand cards, read bidStr)
                _ -> Nothing

newCardFromChar :: Char -> Maybe NewCard
newCardFromChar '2' = Just NewTwo
newCardFromChar '3' = Just NewThree
newCardFromChar '4' = Just NewFour
newCardFromChar '5' = Just NewFive
newCardFromChar '6' = Just NewSix
newCardFromChar '7' = Just NewSeven
newCardFromChar '8' = Just NewEight
newCardFromChar '9' = Just NewNine
newCardFromChar 'T' = Just NewT
newCardFromChar 'J' = Just NewJ
newCardFromChar 'Q' = Just NewQ
newCardFromChar 'K' = Just NewK
newCardFromChar 'A' = Just NewA
newCardFromChar _ = Nothing
