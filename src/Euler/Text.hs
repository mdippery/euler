{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Text
  Description : Utility functions for working with text
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Miscellaneous functions for working with text.
-}
module Euler.Text where

import Data.Bool (bool)
import Data.Char (ord)

import Euler.Math (triangleNumbers)

-- | A set of characters
data CharacterSet = CharacterSet [Char]

-- | True if a set of characters contains the given character.
contains :: CharacterSet -> Char -> Bool
contains (CharacterSet set) ch = ch `elem` set

-- | Value of a letter, starting with 1 for 'A'.
letterValue :: Char -> Int
letterValue = flip (-) 64 . ord

-- | Sum of the value of each letter in a string.
stringValue :: String -> Int
stringValue = sum . map letterValue

-- | Removes characters in the given character set from a string.
removeCharacters :: CharacterSet  -- ^ Character set
                 -> String        -- ^ Original string
                 -> String        -- ^ String with characters removed
removeCharacters cs = foldr (\ch memo -> bool (ch : memo) memo (cs `contains` ch)) ""

-- | Number of /letters/ in a string.
--
-- This excludes non-alpha characters.
countLetters :: String -> Integer
countLetters = sum . map (bool 0 1 . flip elem ['a'..'z'])

-- | Converts a string to a list of integers.
ints :: String -> [Int]
ints = map (read . (:""))

-- | Converts a number to its word form.
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

-- | True if the word is a triangle word.
--
-- A "triangle word" is a word whose 'stringValue' is a triangle number,
-- as defined in <https://projecteuler.net/problem=42 Euler Problem #42>.
isTriangleWord :: String -> Bool
isTriangleWord s =
  let v = stringValue s
      tns = takeWhile (<= v) triangleNumbers
   in v `elem` tns
