{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Poker where

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

data Card = Card CardValue Suit deriving (Eq, Show)

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
