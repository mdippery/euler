{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Text where

import Data.Char (ord)
import Euler.Math (triangleNumbers)

data CharacterSet = CharacterSet [Char]

contains :: CharacterSet -> Char -> Bool
contains (CharacterSet set) ch = ch `elem` set

letterValue :: Char -> Int
letterValue = flip (-) 64 . ord

stringValue :: String -> Int
stringValue = sum . map letterValue

removeCharacters :: CharacterSet -> String -> String
removeCharacters _ "" = ""
removeCharacters set (ch:rest)
  | set `contains` ch = removeCharacters set rest
  | otherwise = ch : removeCharacters set rest

countLetters :: String -> Integer
countLetters = (sum . map cl)
  where cl ch | ch `elem` ['a'..'z'] = 1
              | otherwise            = 0

ints :: String -> [Int]
ints = map (read . (:""))

toWord :: Integer -> String
toWord 1    = "one"
toWord 2    = "two"
toWord 3    = "three"
toWord 4    = "four"
toWord 5    = "five"
toWord 6    = "six"
toWord 7    = "seven"
toWord 8    = "eight"
toWord 9    = "nine"
toWord 10   = "ten"
toWord 11   = "eleven"
toWord 12   = "twelve"
toWord 13   = "thirteen"
toWord 14   = "fourteen"
toWord 15   = "fifteen"
toWord 16   = "sixteen"
toWord 17   = "seventeen"
toWord 18   = "eighteen"
toWord 19   = "nineteen"
toWord 20   = "twenty"
toWord 30   = "thirty"
toWord 40   = "forty"
toWord 50   = "fifty"
toWord 60   = "sixty"
toWord 70   = "seventy"
toWord 80   = "eighty"
toWord 90   = "ninety"
toWord 1000 = "one thousand"
toWord n
  | n < 100   = toWord (n - r10) ++ "-" ++ toWord r10
  | r100 == 0 = toWord (n `div` 100) ++ " hundred"
  | n < 1000  = toWord ((n `div` 100) * 100) ++ " and " ++ toWord r100
  where r10 = n `rem` 10
        r100 = n `rem` 100

isTriangleWord :: String -> Bool
isTriangleWord s =
  let v = stringValue s
      tns = takeWhile (<= v) triangleNumbers
   in v `elem` tns
