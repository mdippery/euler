{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Bool
  Description : Boolean functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides missing boolean functionality.
-}
module Euler.Bool where

($||) x = any ($ x)
