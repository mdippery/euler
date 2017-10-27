module Euler.Util where

pairWithFunc :: (a -> b) -> a -> (a, b)
pairWithFunc f x = (x, f x)

pairHasEqualElements :: Eq a => (a, a) -> Bool
pairHasEqualElements (x, y) = x == y
