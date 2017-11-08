{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Calendar where

import Data.Dates ( DateInterval(..)
                  , DateTime
                  , addInterval
                  , dateWeekDay
                  , day
                  , minusInterval
                  , nextMonday
                  , weekdayNumber)

isSunday :: DateTime -> Bool
isSunday = (== 7) . weekdayNumber . dateWeekDay

isFirstOfMonth :: DateTime -> Bool
isFirstOfMonth = (== 1) . day

nextSunday :: DateTime -> DateTime
nextSunday dt
  | isSunday dt = dt `addInterval` Weeks 1
  | otherwise = nextMonday dt `minusInterval` Days 1

sundaysBetween :: DateTime -> DateTime -> [DateTime]
sundaysBetween start end
  | start > end = []
  | isSunday start = start : sundaysBetween (nextSunday start) end
  | otherwise = sundaysBetween (nextSunday start) end
