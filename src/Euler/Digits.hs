{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Digits
  Description : Various functions for working with digits of integral numbers
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Various functions for working with digits, including the conversion of
  integral numbers to and from individual digits.
-}
module Euler.Digits
  (
    -- * Numbers
    allDigits
  , digits
  , sameDigits
  , unDigits
  ) where

import Data.List (sort)

import qualified Data.Digits as D

-- | Converts a number into a list of digits.
--
-- When converting 0, an empty list is returned. When converting a negative
-- number, the digits of a number are returned, but all of them are negative
-- (except for zeroes).
--
-- This is the same as calling @'D.digits' 10@.
--
-- ==== Examples
--
-- >>> (undigits . digits) 123
-- 123
--
-- >>> digits 0
-- []
--
-- >>> digits (-1024)
-- [-1,0,-2,-4]
digits
  :: Integral a
  => a   -- ^ Value to convert to individual digits
  -> [a] -- ^ List of digits in base 10
digits = D.digits 10

-- | Converts a list of digits into a number.
--
-- When given an empty list, 0 is returned, which maintains parity with
-- 'digits'. When a given list in which all digits (save for 0) are negative,
-- a negative integer is returned.
--
-- Do not pass a list containing a mix of negative and positive digits. The
-- function call will work, but the result will likely be unexpected.
--
-- This is the same as calling @'D.unDigits' 10@.
--
-- ==== Examples
--
-- >>> (digits . unDigits) [1,2,3]
-- [1,2,3]
--
-- >>> unDigits 0
-- []
--
-- >>> unDigits [-1,0,-2,-4]
-- 1024
unDigits
  :: Integral a
  => [a]   -- ^ List of digits in base 10
  -> a     -- ^ Number represented by the list of digits
unDigits = D.unDigits 10

-- | Converts a number into a list of digits, sorted in ascending
-- order.
allDigits
  :: Integral a
  => a      -- ^ Value to convert to individual digits
  -> [a]    -- ^ List of digits in base 10, sorted in ascending order
allDigits = sort . digits

-- | True if both numbers contain exactly the same digits.
sameDigits :: Integral a => a -> a -> Bool
sameDigits = (. allDigits) . (==) . allDigits
