module Utils where

import Date exposing ( Date )
import Result

getDate : Result String Date -> Date
getDate result =
  case result of
    Result.Ok value -> value

getMaybe : Maybe a -> a
getMaybe maybe =
  case maybe of
    Maybe.Just value -> value
