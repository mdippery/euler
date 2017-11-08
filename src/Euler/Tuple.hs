module Euler.Tuple where

mapT :: (a -> b -> c) -> [(a, b)] -> [c]
mapT f = map (\(a,b) -> f a b)

zipT :: (a -> b) -> [a] -> [(a, b)]
zipT f = map (\x -> (x, f x))

unzipT :: [(a, b)] -> ([a], [b])
unzipT = unzip

equalT :: Eq a => (a, a) -> Bool
equalT (x, y) = x == y
