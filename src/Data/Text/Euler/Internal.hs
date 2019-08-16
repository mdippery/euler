{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Data.Text.Euler.Internal
  Description : Utility functions for working with text
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Miscellaneous functions for working with text; specifically for solving
  certain Project Euler problems.
-}
module Data.Text.Euler.Internal where

import Data.Text.Euler (stringValue)
import Euler.Math (isTriangle)

-- | True if the word is a triangle word.
--
-- A "triangle word" is a word whose 'stringValue' is a triangle number,
-- as defined in <https://projecteuler.net/problem=42 Euler Problem #42>.
isTriangleWord :: String -> Bool
isTriangleWord = isTriangle . toInteger . stringValue
