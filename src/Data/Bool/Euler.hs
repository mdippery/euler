{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Data.Bool.Euler
  Description : Boolean functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides missing boolean functionality.
-}
module Data.Bool.Euler where

-- | Chains the given foldable together using the '||' operator and returns
-- the result.
--
-- ==== Examples
--
-- >>> 1 $|| [(== 1), (== 2), (== 3)]
-- True
-- >>> 1 $|| [(== 2), (== 3)]
-- False
($||) :: Foldable t => a -> t (a -> Bool) -> Bool
($||) x = any ($ x)
