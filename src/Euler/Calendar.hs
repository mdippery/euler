{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Calendar
  Description : Useful calendar functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Useful date-related functions.
-}
module Euler.Calendar where

import Data.List (unfoldr)
import Data.Dates ( DateInterval(..)
                  , DateTime
                  , addInterval
                  , dateWeekDay
                  , day
                  , minusInterval
                  , nextMonday
                  , weekdayNumber)

-- | Returns true if the given date/time is a Sunday.
isSunday :: DateTime  -- ^ Date/time
         -> Bool      -- ^ True if the given date/time is a Sunday
isSunday = (== 7) . weekdayNumber . dateWeekDay

-- | Returns true if the given date/time lies on the first of a month.
isFirstOfMonth :: DateTime  -- ^ Date/time
               -> Bool      -- ^ True if the date/time is first day of month
isFirstOfMonth = (== 1) . day

-- | Returns the next Sunday after the given date/time.
nextSunday :: DateTime  -- ^ Date/time
           -> DateTime  -- ^ First Sunday after the given date/time
nextSunday dt
  | isSunday dt = dt `addInterval` Weeks 1
  | otherwise = nextMonday dt `minusInterval` Days 1

-- | A list of all the Sundays between the given two dates
sundaysBetween :: DateTime    -- ^ Starting date
               -> DateTime    -- ^ Ending date
               -> [DateTime]  -- ^ All Sundays between the two dates
sundaysBetween start end = unfoldr next start'
  where
    start'
      | isSunday start = start
      | otherwise = nextSunday start
    next d
      | d > end = Nothing
      | otherwise = Just (d, nextSunday d)
