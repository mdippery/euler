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
               deriving (Eq, Ord, Show)

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

getCards :: PlayerHand -> [Card]
getCards (PlayerHand _ c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]

sameSuit :: PlayerHand -> Bool
sameSuit ph =
  let cards = getCards ph
      suits = map cardSuit cards
      suit = head suits
   in all (== suit) suits

nKind :: Int -> PlayerHand -> Bool
nKind n = elem n . map length . groupBy ((==) `on` cardValue) . sort . getCards
