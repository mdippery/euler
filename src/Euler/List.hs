{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.List where

(<:) :: [a] -> a -> [a]
as <: b = as ++ [b]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

zipWithIndex :: (Enum b, Num b) => [a] -> [(b, a)]
zipWithIndex = zip [0..]

unzipWithIndex :: [(b, a)] -> [a]
unzipWithIndex = map snd

dropNth :: Integral b => b -> [a] -> [a]
dropNth n =
  let step n (i, x) acc = if i `rem` n == 0 then acc else (i, x):acc
   in unzipWithIndex . foldr (step n) [] . zipWithIndex

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i e ls =
  case splitAt i ls of
    ([], []) -> []
    (h, []) -> ls
    (h, rest) -> h ++ [e] ++ tail rest

windows :: Int -> [a] -> [[a]]
windows n [] = []
windows n ls
  | length ls >= n = take n ls : windows n (tail ls)
  | otherwise      = []

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n ls =
  let (h,rest) = splitAt n ls
   in h : splitEvery n rest
