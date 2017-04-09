module Main where

import Data.List.Split (splitOn)
import Data.List (sortOn)
import Data.List.Unique (unique, count)

main :: IO ()
main = do
  putStrLn "Give me a hand of 5 cards please! (We expectg good input): "
  hand <- getLine
  putStrLn $ "Your best hand is "++(parseAndFindBestCombination hand)

parseAndFindBestCombination :: String -> String
parseAndFindBestCombination = bestCombination . parseAndSortCards

bestCombination :: Hand -> String
bestCombination hand =
  if isStraight hand && isFlush hand then "Straight Flush"
  else if maxDuplicates hand == 4 then "Four of a Kind"
  else if isFullHouse hand then "Full House"
  else if isFlush hand then "Flush"
  else if isStraight hand then "Straight"
  else if maxDuplicates hand == 3 then "Three of a Kind"
  else if isTwoPair hand then "Two Pair"
  else if maxDuplicates hand == 2 then "Pair"
  else "High Card"

isTwoPair :: Hand -> Bool
isTwoPair hand = case duplicates hand of
  [(_, 2), (_, 2), (_, 1)] -> True
  [(_, 2), (_, 1), (_, 2)] -> True
  [(_, 1), (_, 2), (_, 2)] -> True
  _ -> False

isFullHouse :: Hand -> Bool
isFullHouse hand = case duplicates hand of
  [(_, 2), (_, 3)] -> True
  [(_, 3), (_, 2)] -> True
  _ -> False

isFlush :: Hand -> Bool
isFlush hand = let
  suits = map suit hand
  firstSuit = suits!!0
  ofSameSuit = length (filter (==firstSuit) suits)
  in if ofSameSuit == (length hand) then True else False

isStraight :: Hand -> Bool
isStraight hand = let
  values = map value hand
  f h prev =
    case h of
      [] -> True
      x:xs -> if x == prev + 1
        then f xs x
        else False
  in f (drop 1 values) (values!!0)

data Card = Card {
  value :: Int,
  suit :: String
} deriving (Eq, Show)

instance Ord Card where
  a < b = (value a) < (value b)
  a > b = (value a) > (value b)
  a >= b = (value a) >= (value b)
  a <= b = (value a) <= (value b)
  max a b = if a > b then a else b
  min a b = if a > b then b else a

type Hand = [Card]

maxDuplicates :: Hand -> Int
maxDuplicates hand = let
  dupes = map (snd) $ duplicates hand
  in foldl max 0 dupes

duplicates :: Hand -> [(Int, Int)]
duplicates hand = count $ map value hand

parseCards :: String -> Hand
parseCards hand =
  let
    getSuit c = drop ((length c) - 1) c
    getValue c = let s = take ((length c) - 1) c in
      case s of
         "A" -> 14
         "K" -> 13
         "Q" -> 12
         "J" -> 11
         _ -> read s
    parseCard c = Card (getValue c) (getSuit c)
    unparsedCards hand = splitOn " " hand
  in map parseCard (unparsedCards hand)

sortCards :: Hand -> Hand
sortCards c = sortOn (\c -> (value c)) c

parseAndSortCards :: String -> Hand
parseAndSortCards = sortCards . parseCards
