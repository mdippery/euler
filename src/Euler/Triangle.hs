{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Triangle
  Description : General functions for working with triangles
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Calculations involving geometric triangles.
-}
module Euler.Triangle
  (
    -- * Operations and calculations
    triangleArea
  , trianglePerimeter

    -- * Sequences and generators
  , rightTriangles
  ) where

import Control.Monad (liftM2)
import Data.List (nub)

import Euler.Tuple (sort3)

-- | Generates triplets for a right triangle of the given perimeter.
rightTriangles :: Integral a
               => a           -- ^ Perimeter
               -> [(a,a,a)]   -- ^ Solutions for right triangles of the given perimeter
rightTriangles p = nub $ map sort3 $ filter (liftM2 (&&) sqfits pfits) [(a, b a, c a) | a <- [1..u]]
  where
    u = p `div` 3
    b a = (p ^ 2 - 2 * p * a) `div` (2 * p - 2 * a)
    c a = p - b a - a
    pfits (a,b,c) = a + b + c == p
    sqfits (a,b,c) = c ^ 2 == a ^ 2 + b ^ 2

-- | Area of a triangle given its three sides.
triangleArea :: Floating a => (a,a,a) -> a
triangleArea (a,b,c) =
  let p = (a + b + c) / 2
      pa = p - a
      pb = p - b
      pc = p - c
   in sqrt (p * pa * pb * pc)

-- | Perimeter of a triangle given its three sides.
trianglePerimeter :: Num a => (a,a,a) -> a
trianglePerimeter (a,b,c) = a + b + c
