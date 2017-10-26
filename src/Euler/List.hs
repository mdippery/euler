module Euler.List where

zipWithIndex :: [a] -> [(Integer, a)]
zipWithIndex = zip [0..]

unzipWithIndex :: [(Integer, a)] -> [a]
unzipWithIndex = map (\(i, x) -> x)

dropNth :: Integer -> [a] -> [a]
dropNth n =
  let step n (i, x) acc = if i `rem` n == 0 then acc else (i, x):acc
   in unzipWithIndex . foldr (step n) [] . zipWithIndex
