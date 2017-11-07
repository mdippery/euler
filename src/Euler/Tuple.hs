module Euler.Tuple where

compose :: (a -> b) -> a -> (a, b)
compose f x = (x, f x)

decompose :: (a, b) -> a
decompose = fst

equal :: Eq a => (a, a) -> Bool
equal (x, y) = x == y
