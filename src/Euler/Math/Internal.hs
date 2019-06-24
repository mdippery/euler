{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Math.Internal
  Description : Solutions for math equations specific to Project Euler
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Helps solve various math-heavy problems.
-}
module Euler.Math.Internal where

import Euler.Math (extract3)

-- | Retrieves all 3-digit "substrings" of a number, starting at the
-- given indexes.
--
-- Specifically, this function is useful in solving
-- <https://projecteuler.net/problem=43 Euler Problem #43>.
--
-- ==== Examples
-- >>> let x = 1406357289
-- >>> numericalSubstrings x [2..8]
-- [406,63,635,357,572,728,289]
numericalSubstrings :: Integral a
                    => a      -- ^ Base number
                    -> [a]    -- ^ Indexes from which 3-digit "substrings" should be extracted
                    -> [a]    -- ^ All 3-digit substrings for the given indexes
numericalSubstrings = map . flip extract3

-- | True if a number fits the divisibility rules as defined in
-- <https://projecteuler.net/problem=43 Euler Problem #43>.
hasDivisibilityProperty :: Integral a => a -> Bool
hasDivisibilityProperty x =
  let ixs = [2..8]
      ds  = [2,3,5,7,11,13,17]
   in all (== 0) $ zipWith mod (numericalSubstrings x ixs) ds

-- | True if the fraction "cancels" according to the rules outlined in
-- <https://projecteuler.net/problem=33 Euler Problem #33>.
cancelsUnorthodoxically :: (Num a, Eq a)
                        => a      -- ^ Digit multiplier
                        -> a      -- ^ Digit in numerator
                        -> a      -- ^ Digit in denominator
                        -> Bool   -- ^ True if the fraction "cancels"
cancelsUnorthodoxically i n d = d * (10 * n + i) == n * (10 * i + d)
