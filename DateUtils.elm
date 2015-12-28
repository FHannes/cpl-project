module DateUtils where

{-| This module provides date (and time) utilities.

@docs now, getDate, dateToString

-}

import Date exposing ( Date )
import Date.Format
import Time exposing ( Time )

import Native.DateUtils

{-| Returns the current time.
-}
now : Time
now =
  Native.DateUtils.now

{-| Returns the current date.
-}
getDate : Date
getDate =
  Date.fromTime now

{-| Converts a date to a string.
-}
dateToString : Date -> String
dateToString date =
  Date.Format.format "%Y-%m-%d" date
