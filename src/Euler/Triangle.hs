{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Triangle
  Description : General functions for working with triangles
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Easily work with and generate triangles.
-}
module Euler.Triangle
  (
    -- * Sequences and generators
    rightTriangles
  ) where

import Data.List (nub, sort)

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
