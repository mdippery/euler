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
    -- * Basic properties
    isIntegralTriangle

    -- * Operations and calculations
  , triangleArea
  , trianglePerimeter

    -- * Sequences and generators
  , rightTriangles
  ) where

import Data.List (nub, sort)
import Euler.Math (isInteger)

-- | Generates triplets for a right triangle of the given perimeter.
rightTriangles :: Integral a
               => a           -- ^ Perimeter
               -> [(a,a,a)]   -- ^ Solutions for right triangles of the given perimeter
rightTriangles p = map toT $ nub $ map sort $ filter sqfits $ filter pfits [[a, b a, c a] | a <- [1..u]]
  where
    u = p `div` 3
    b a = (p ^ 2 - 2 * p * a) `div` (2 * p - 2 * a)
    c a = p - b a - a
    pfits = (== p) . sum
    sqfits ns = (ns !! 2) ^ 2 == (ns !! 0) ^ 2 + (ns !! 1) ^ 2
    toT ns = (ns !! 0, ns !! 1, ns !! 2)

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

-- | True if the triangle defined by its three sides has integral side
-- lengths and integral perimeter.
isIntegralTriangle :: (RealFrac a, Floating a) => (a,a,a) -> Bool
isIntegralTriangle t@(a,b,c) = isInteger a && isInteger b && isInteger c && (isInteger . triangleArea) t
