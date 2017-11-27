{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Tuple where

import Control.Monad (ap)

mapT :: (a -> b -> c) -> [(a, b)] -> [c]
mapT = map . uncurry

zipT :: (a -> b) -> [a] -> [(a, b)]
zipT = map . ap (,)

unzipT :: [(a, b)] -> ([a], [b])
unzipT = unzip

flattenT :: [(a, a)] -> [a]
flattenT = foldr (\(a, b) memo -> memo ++ [a,b]) []

equalT :: Eq a => (a, a) -> Bool
equalT = uncurry (==)
