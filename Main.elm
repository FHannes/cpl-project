module Main where

import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import Signal

import Manager
import Static exposing ( Email )

-- Name: Frédéric Hannes
-- Student ID: S0218251


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Come up with your own extension!
-- http://getbootstrap.com/components/#pagination ?
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- Start of program

data : Manager.Model
data =
  let mails = Manager.addMails Static.emails Manager.init
  in Manager.updModel <| Manager.addTodos Static.reminders mails

mailbox : Signal.Mailbox (Maybe Manager.Action)
mailbox = Signal.mailbox Nothing

state : Signal Manager.Model
state =
  let update action model =
    case action of
      Just a -> Manager.update a model
      _ -> model
  in Signal.foldp update data mailbox.signal

main : Signal Html
main =
  let view = Manager.view (Signal.forwardTo mailbox.address Just)
  in Signal.map view state
