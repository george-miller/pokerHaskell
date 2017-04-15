{-
This file is part of pokerHaskell.

pokerHaskell is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

pokerHaskell is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with pokerHaskell.  If not, see <http://www.gnu.org/licenses/>.
-}
module Main where

import Data.List.Split (splitOn)
import Data.List (sortOn)
import Data.List.Unique (unique, count)

main :: IO ()
main = do
  putStrLn "Give me a hand of 5 cards please! (We expect good input): "
  hand <- getLine
  putStrLn $ "Your best hand is "++parseAndFindBestCombination hand

parseAndFindBestCombination :: String -> String
parseAndFindBestCombination = bestCombination . parseAndSortCards

bestCombination :: Hand -> String
bestCombination hand
  | isStraight hand && isFlush hand = "Straight Flush"
  | maxDuplicates hand == 4 = "Four of a Kind"
  | isFullHouse hand = "Full House"
  | isFlush hand = "Flush"
  | isStraight hand = "Straight"
  | maxDuplicates hand == 3 = "Three of a Kind"
  | isTwoPair hand = "Two Pair"
  | maxDuplicates hand == 2 = "Pair"
  | otherwise = "High Card"

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
  firstSuit = head suits
  ofSameSuit = length (filter (==firstSuit) (tail suits))
  in ofSameSuit == length hand

isStraight :: Hand -> Bool
isStraight hand = let
  values = map value hand
  f h prev =
    case h of
      [] -> True
      x:xs -> (x == prev + 1) && f xs x
  in f (tail values) (head values)

data Card = Card {
  value :: Int,
  suit :: String
} deriving (Eq, Show)

instance Ord Card where
  a < b = value a < value b
  a > b = value a > value b
  a >= b = value a >= value b
  a <= b = value a <= value b
  max a b = if a > b then a else b
  min a b = if a > b then b else a

type Hand = [Card]

maxDuplicates :: Hand -> Int
maxDuplicates hand = let
  dupes = map snd $ duplicates hand
  in foldl max 0 dupes

duplicates :: Hand -> [(Int, Int)]
duplicates hand = count $ map value hand

parseCards :: String -> Hand
parseCards hand =
  let
    getSuit c = drop (length c - 1) c
    getValue c = let s = take (length c - 1) c in
      case s of
         "A" -> 14
         "K" -> 13
         "Q" -> 12
         "J" -> 11
         _ -> read s
    parseCard c = Card (getValue c) (getSuit c)
    unparsedCards = splitOn " " hand
  in map parseCard unparsedCards

sortCards :: Hand -> Hand
sortCards = sortOn value

parseAndSortCards :: String -> Hand
parseAndSortCards = sortCards . parseCards
