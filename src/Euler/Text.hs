module Euler.Text where

import Data.Char (ord)

letterValue :: Char -> Int
letterValue ch = ord ch - 64

stringValue :: String -> Int
stringValue = sum . map letterValue
