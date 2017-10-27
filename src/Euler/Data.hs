module Euler.Data where

import qualified Data.Digits as D

digits :: Integral a => a -> [a]
digits = D.digits 10
