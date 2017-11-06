{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Util where

pairWithFunc :: (a -> b) -> a -> (a, b)
pairWithFunc f x = (x, f x)

pairHasEqualElements :: Eq a => (a, a) -> Bool
pairHasEqualElements (x, y) = x == y
