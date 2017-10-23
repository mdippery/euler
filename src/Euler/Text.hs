module Euler.Text where

toDigit ch = read [ch] :: Int

digits n = map toDigit (show n)
