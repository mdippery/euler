{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Poker
  Description : Poker game simulation
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides functions and data types for simulating a game of poker and
  calculating the winner.
-}
module Euler.Poker
  (
    -- * Data types
    Card (..)
  , CardValue (..)
  , HandType (..)
  , Player (..)
  , PlayerHand (..)
  , Suit (..)

    -- * Accessors
  , cards
  , orderedCards
  , player

    -- * Game parsing
  , parseCard
  , parseCardSuit
  , parseCardValue
  , parseGame
  , parseHand
  , winner

    -- * Hand detection
  , isHighCard
  , isOnePair
  , isTwoPair
  , isThreeKind
  , isStraight
  , isFlush
  , isFullHouse
  , isFourKind
  , isStraightFlush
  , isRoyalFlush
  , playerHandType
  ) where

import Data.Function (on)
import Data.List (groupBy, sort, sortBy)

-- | A playing card suit
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Show)

-- | A playing card value
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

-- | A playing card
data Card = Card
  { cardValue :: CardValue  -- ^ Value of the playing card
  , cardSuit :: Suit        -- ^ The playing card's suit
  } deriving (Eq, Show)

-- | Defines the ordering of a card
instance Ord Card where
  compare (Card left _) (Card right _) = left `compare` right

-- | A type of poker hand
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

-- | Identifies a player
data Player = PlayerOne | PlayerTwo deriving (Eq, Show)

-- | Cards held by a player
data PlayerHand = PlayerHand Player Card Card Card Card Card deriving (Eq, Show)

-- | Retrieves the owner of a player's card
player :: PlayerHand -> Player
player (PlayerHand p _ _ _ _ _) = p

-- | Retrieve's a player's cards as a list of cards
cards :: PlayerHand -> [Card]
cards (PlayerHand _ c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]

-- | Groups a player's card by the value of the card
groupedCardValues :: PlayerHand -> [[Card]]
groupedCardValues = groupBy ((==) `on` cardValue) . sort . cards

-- | Most valuable card in a set of cards
highestCardOf :: [Card] -> Card
highestCardOf = minimum

-- | True if all cards in the player's hand are of the same suit
isSameSuit :: PlayerHand -> Bool
isSameSuit ph =
  let cs = cards ph
      suits = map cardSuit cs
      suit = head suits
   in all (== suit) suits

-- | True if all the cards in the player's hand are of consecutive values
isConsecutive :: PlayerHand -> Bool
isConsecutive ph =
  let cs = cards ph
      vals = (sort . map cardValue) cs
      start = head vals
   in vals == take 5 [start..]

-- | True if the hand contains /n/ cards of the same suit
nKind :: Int          -- ^ Target number of cards of the same suit
      -> PlayerHand   -- ^ Player's hand
      -> Bool         -- ^ True if the hand contains /n/ cards of the same suit
nKind n = elem n . map length . groupedCardValues

-- | True if the best hand is a "high card"
isHighCard :: PlayerHand -> Bool
isHighCard ph =
  let total = (length . cards) ph
      unique = (length . groupedCardValues) ph
   in unique == total && (not . isStraight) ph

-- | True if the hand contains /n/ pairs
hasPairs :: Int         -- ^ Target number of pairs
         -> PlayerHand  -- ^ Player hand
         -> Bool        -- ^ True if the hand contains /n/ pairs
hasPairs n = (== n) . length . filter (== 2) . map length . groupedCardValues

-- | True if the player's hand contains a pair
isOnePair :: PlayerHand -> Bool
isOnePair = hasPairs 1

-- | True if the player's hand contains two pairs
isTwoPair :: PlayerHand -> Bool
isTwoPair = hasPairs 2

-- | True if the player's hand contains a three of a kind
isThreeKind :: PlayerHand -> Bool
isThreeKind ph = nKind 3 ph && (not . isOnePair) ph

-- | True if the player's hand is a straight
isStraight :: PlayerHand -> Bool
isStraight = isConsecutive

-- | True if the player's hand is a flush
isFlush :: PlayerHand -> Bool
isFlush = isSameSuit

-- | True if the player's hand is a full house
isFullHouse :: PlayerHand -> Bool
isFullHouse ph = nKind 3 ph && isOnePair ph

-- | True if the player's hand is a four of a kind
isFourKind :: PlayerHand -> Bool
isFourKind = nKind 4

-- | True if the player's hand is a straight flush
isStraightFlush :: PlayerHand -> Bool
isStraightFlush ph = isStraight ph && isFlush ph && (not . isRoyalFlush) ph

-- | True if the player's hand is a royal flush
isRoyalFlush :: PlayerHand -> Bool
isRoyalFlush ph =
  let lowCard = (cardValue . minimum . cards) ph
   in isStraight ph && isFlush ph && lowCard == Ten

-- | Type of a player's hand
playerHandType :: PlayerHand -> HandType
playerHandType ph
  | isRoyalFlush ph = RoyalFlush
  | isStraightFlush ph = StraightFlush
  | isFourKind ph = FourOfAKind
  | isFullHouse ph = FullHouse
  | isFlush ph = Flush
  | isStraight ph = Straight
  | isThreeKind ph = ThreeOfAKind
  | isTwoPair ph = TwoPairs
  | isOnePair ph = OnePair
  | isHighCard ph = HighCard

-- | Converts a character into a card value
parseCardValue :: Char -> CardValue
parseCardValue '2' = Two
parseCardValue '3' = Three
parseCardValue '4' = Four
parseCardValue '5' = Five
parseCardValue '6' = Six
parseCardValue '7' = Seven
parseCardValue '8' = Eight
parseCardValue '9' = Nine
parseCardValue 'T' = Ten
parseCardValue 'J' = Jack
parseCardValue 'Q' = Queen
parseCardValue 'K' = King
parseCardValue 'A' = Ace

-- | Converts a letter into a card suit
parseCardSuit :: Char -> Suit
parseCardSuit 'C' = Clubs
parseCardSuit 'D' = Diamonds
parseCardSuit 'H' = Hearts
parseCardSuit 'S' = Spades

-- | Return's a player's cards, ordered by value
orderedCards :: PlayerHand -> [Card]
orderedCards = map head . reverse . sortBy (compare `on` length) . groupedCardValues

-- | Resolves a tie between two player hands.
--
-- A tie occurs when players have the same type of hand, in which case,
-- the winner is the player with the highest card value.
resolveTie :: PlayerHand  -- ^ Player One's hand
           -> PlayerHand  -- ^ Player Two's hand
           -> Player      -- ^ Winning player
resolveTie ph1 ph2
  | cs1 > cs2 = PlayerOne
  | cs2 > cs1 = PlayerTwo
  where
    cs1 = orderedCards ph1
    cs2 = orderedCards ph2

-- | Determines the winner of a hand of poker
winner :: PlayerHand  -- ^ Player One's hand
       -> PlayerHand  -- ^ Player Two's hand
       -> Player      -- ^ Winning player
winner ph1 ph2
  | ht1 > ht2 = PlayerOne
  | ht2 > ht1 = PlayerTwo
  | ht1 == ht2 = resolveTie ph1 ph2
  where
    ht1 = playerHandType ph1
    ht2 = playerHandType ph2

-- | Parse a card string into a card.
--
-- Card strings are in the form "4D", where the first character is the
-- value of the card, and the second is the value of the suit.
parseCard :: String -> Card
parseCard [v,s] = Card (parseCardValue v) (parseCardSuit s)

-- | Parses a player's hand string into a hand.
--
-- Hand strings are in the format "4A QD KS 3H 2S", where each pair of
-- characters represents the value of a card.
parseHand :: Player       -- ^ Player who owns the hand
          -> [String]     -- ^ List of card values
          -> PlayerHand   -- ^ Parsed hand
parseHand p ss =
  let cs = map parseCard ss
   in PlayerHand p (head cs) (cs !! 1) (cs !! 2) (cs !! 3) (cs !! 4)

-- | Parses a poker game into a pair of hands.
--
-- The first element of the tuple is Player One's hand, and the second
-- element is Player Two's hand.
parseGame :: String -> (PlayerHand, PlayerHand)
parseGame s =
  let cs = words s
      p1s = take 5 cs
      p2s = drop 5 cs
   in (parseHand PlayerOne p1s, parseHand PlayerTwo p2s)
