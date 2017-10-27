module Euler.Text where

import Data.Char (ord)

toDigit :: Char -> Integer
toDigit ch = read [ch]

digits :: Integer -> [Integer]
digits n = map toDigit (show n)

letterValue :: Char -> Int
letterValue ch = ord ch - 64

stringValue :: String -> Int
stringValue = sum . map letterValue
