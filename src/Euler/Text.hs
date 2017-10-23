module Euler.Text where

toDigit ch = read [ch]

digits n = map toDigit (show n)
