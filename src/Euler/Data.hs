{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Data
  Description : Various functions for working with general datatypes
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Various functions for working with general datatypes.
-}
module Euler.Data where

import qualified Data.Digits as D

-- | Converts a number into a list of digits.
--
-- ==== Examples
--
-- > (undigits . digits) n == n
digits :: Integral a
       => a   -- ^ Value to convert to individual digits
       -> [a] -- ^ List of digits in base 10
digits = D.digits 10

-- | Converts a list of digits into a number.
--
-- ==== Examples
--
-- > (digits . undigits) [1,2,3] == [1,2,3]
unDigits :: Integral a
         => [a]   -- ^ List of digits in base 10
         -> a     -- ^ Number represented by the list of digits
unDigits = D.unDigits 10
