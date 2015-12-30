module Storage where

import Json.Encode exposing ( .. )
import Maybe
import Date exposing ( Date )

import Static exposing ( Email, Reminder )

encodeMaybeDate : Maybe Date -> Value
encodeMaybeDate mDate =
  case mDate of
    Nothing -> string ""
    Just date -> float <| Date.toTime date

encodeEmail : Maybe Date -> Email -> Value
encodeEmail snoozeDate mail =
  object
    [ ("from", string mail.from )
    , ("to", string mail.to )
    , ("title", string mail.title )
    , ("body", string mail.body )
    , ("date", string mail.date )
    , ("sooze", encodeMaybeDate snoozeDate)
    ]

encodeReminder : Maybe Date -> Reminder -> Value
encodeReminder  snoozeDate reminder =
  object
    [ ("body", string reminder.body )
    , ("created", string reminder.created )
    , ("deadline", string reminder.deadline )
    , ("sooze", encodeMaybeDate snoozeDate)
    ]
