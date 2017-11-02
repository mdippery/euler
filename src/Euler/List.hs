module Euler.List where

(<:) :: [a] -> a -> [a]
as <: b = as ++ [b]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

zipWithIndex :: (Enum b, Num b) => [a] -> [(b, a)]
zipWithIndex = zip [0..]

unzipWithIndex :: [(b, a)] -> [a]
unzipWithIndex = map (\(i, x) -> x)

dropNth :: Integral b => b -> [a] -> [a]
dropNth n =
  let step n (i, x) acc = if i `rem` n == 0 then acc else (i, x):acc
   in unzipWithIndex . foldr (step n) [] . zipWithIndex
