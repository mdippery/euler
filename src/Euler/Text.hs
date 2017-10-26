module Euler.Text where

toDigit :: Char -> Integer
toDigit ch = read [ch]

digits :: Integer -> [Integer]
digits n = map toDigit (show n)
