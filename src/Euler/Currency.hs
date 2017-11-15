{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Currency where

type Cents = Integer

data EnglishPence = OnePence
                  | TwoPence
                  | FivePence
                  | TenPence
                  | TwentyPence
                  | FiftyPence
                  | HundredPence
                  | TwoHundredPence
  deriving (Enum, Eq, Show)

class Currency a where
  amount :: a -> Cents

instance Currency EnglishPence where
  amount OnePence = 1
  amount TwoPence = 2
  amount FivePence = 5
  amount TenPence = 10
  amount TwentyPence = 20
  amount FiftyPence = 50
  amount HundredPence = 100
  amount TwoHundredPence = 200

coinCombos :: Cents -> EnglishPence -> Integer
coinCombos 0 _ = 1
coinCombos 1 _ = 1
coinCombos _ OnePence = 1
coinCombos target maxDenom
  | amount maxDenom <= target = coinCombos target (pred maxDenom) + coinCombos (target - amount maxDenom) maxDenom
  | otherwise = coinCombos target (pred maxDenom)
