{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Currency
  Description : Useful currency functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  A handy way to work with currency-like values.
-}
module Euler.Currency where

-- | Base unit of currency
type Cents = Integer

-- | English (UK) unit of currency
data EnglishPence = OnePence
                  | TwoPence
                  | FivePence
                  | TenPence
                  | TwentyPence
                  | FiftyPence
                  | HundredPence
                  | TwoHundredPence
  deriving (Enum, Eq, Show)

-- | A data type that can be used in currency calculations
class Currency a where
  -- | Converts a currency into a base amount that can be used in calculations
  amount :: a -> Cents

-- | Defines the value of each denomination of UK currency, in cents
instance Currency EnglishPence where
  amount OnePence = 1
  amount TwoPence = 2
  amount FivePence = 5
  amount TenPence = 10
  amount TwentyPence = 20
  amount FiftyPence = 50
  amount HundredPence = 100
  amount TwoHundredPence = 200

-- | Calculates the number of ways it is possible to make a given value
-- using denominations up to the maximum specified coin.
--
-- This function can be used to solve <https://projecteuler.net/problem=31 Euler Problem #31>.
-- It is loosely inspired by
-- <http://www.mathblog.dk/project-euler-31-combinations-english-currency-denominations/ this solution>.
coinCombos :: Cents         -- ^ Target amount
           -> EnglishPence  -- ^ Maximum coin denomination
           -> Integer       -- ^ Number of ways to make the target amount
coinCombos 0 _ = 1
coinCombos 1 _ = 1
coinCombos _ OnePence = 1
coinCombos target maxDenom
  | amount maxDenom <= target = coinCombos target (pred maxDenom) + coinCombos (target - amount maxDenom) maxDenom
  | otherwise = coinCombos target (pred maxDenom)
