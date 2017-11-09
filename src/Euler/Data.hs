{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Data where

import qualified Data.Digits as D

digits :: Integral a => a -> [a]
digits = D.digits 10

unDigits :: Integral a => [a] -> a
unDigits = D.unDigits 10
