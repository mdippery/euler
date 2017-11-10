{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Poker where

import Data.Function (on)
import Data.List (groupBy, sort)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Show)

data CardValue = Two
               | Three
               | Four
               | Five
               | Six
               | Seven
               | Eight
               | Nine
               | Ten
               | Jack
               | Queen
               | King
               | Ace
               deriving (Enum, Eq, Ord, Show)

data Card = Card { cardValue :: CardValue
                 , cardSuit :: Suit
                 } deriving (Eq, Show)

instance Ord Card where
  compare (Card left _) (Card right _) = left `compare` right

data HandType = HighCard
              | OnePair
              | TwoPairs
              | ThreeOfAKind
              | Straight
              | Flush
              | FullHouse
              | FourOfAKind
              | StraightFlush
              | RoyalFlush
              deriving (Eq, Ord, Show)

data Hand = Hand { handType :: HandType
                 , highCard :: Card
                 } deriving (Eq, Ord, Show)

data Player = PlayerOne | PlayerTwo deriving Show

data PlayerHand = PlayerHand Player Card Card Card Card Card deriving Show

player :: PlayerHand -> Player
player (PlayerHand p _ _ _ _ _) = p

cards :: PlayerHand -> [Card]
cards (PlayerHand _ c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]

groupedCardValues :: PlayerHand -> [[Card]]
groupedCardValues = groupBy ((==) `on` cardValue) . sort . cards

highestCardOf :: [Card] -> Card
highestCardOf = head . sort

isSameSuit :: PlayerHand -> Bool
isSameSuit ph =
  let cs = cards ph
      suits = map cardSuit cs
      suit = head suits
   in all (== suit) suits

isConsecutive :: PlayerHand -> Bool
isConsecutive ph =
  let cs = cards ph
      vals = (sort . map cardValue) cs
      start = head vals
   in vals == take 5 [start..]

nKind :: Int -> PlayerHand -> Bool
nKind n = elem n . map length . groupedCardValues

isHighCard :: PlayerHand -> Bool
isHighCard = (== 5) . length . groupedCardValues

hasPairs :: Int -> PlayerHand -> Bool
hasPairs n = (== n) . length . filter (== 2) . map length . groupedCardValues

isOnePair :: PlayerHand -> Bool
isOnePair = hasPairs 1

isTwoPair :: PlayerHand -> Bool
isTwoPair = hasPairs 2

isThreeKind :: PlayerHand -> Bool
isThreeKind = nKind 3

isStraight :: PlayerHand -> Bool
isStraight = isConsecutive

isFlush :: PlayerHand -> Bool
isFlush = isSameSuit

isFullHouse :: PlayerHand -> Bool
isFullHouse ph = isThreeKind ph && isOnePair ph

isFourKind :: PlayerHand -> Bool
isFourKind = nKind 4

isStraightFlush :: PlayerHand -> Bool
isStraightFlush ph = isStraight ph && isFlush ph && (not . isRoyalFlush) ph

isRoyalFlush :: PlayerHand -> Bool
isRoyalFlush ph =
  let lowCard = (cardValue . head . sort . cards) ph
   in isStraight ph && isFlush ph && lowCard == Ten
