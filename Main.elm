module Main where

import Char exposing ( KeyCode )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import Http
import Keyboard
import Maybe
import Set exposing ( Set )
import Signal
import Time
import Task exposing ( Task )
import Task.Extra

import MailFetcher exposing ( ServerMails )
import Manager exposing ( Action (..) )
import Static exposing ( Email )

-- Name: Frédéric Hannes
-- Student ID: S0218251


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed
-- Summary:
--   The hotkey Alt+G was added to toggle the visibility of done items. The
--   "To do" and "Done" section headers are only visible when done items are
--   toggled on and at least one item is done.


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed
-- Summary:
--   The hotkey Alt+A was added to toggle the visibility of the "Add Reminder"
--   panel which is hidden by default.


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed
-- Summary:
--   A native module DateUtils.js was added to provide a function which gets
--   the current timestamp in JavaScript. This was wrapped by the Elm module
--   DateUtils. The formatting of the timestamp is provided by the library
--   "mgold/elm-date-format".


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed
-- Summary:
--   A deadline property was added to Static.Reminder. A date field for the
--   deadline was also added to the panel which can be used to add new
--   reminders. Reminder items for which the deadline lies before the current
--   date are marked as EXPIRED in the interface.


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed
-- Summary:
--   A new panel was added which is hidden by default and can be toggled with
--   the hotkey Alt+H. This panel allows you to snooze the currently selected
--   item by providing a snooze date. The system will hide any item which is
--   currently snoozed.


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed
-- Summary:
--   An asynchronous html request was implemented which retrieves the emails
--   from the server on startup. I was unable to retrieve the emails from the
--   original URL due to the browser prevent cross-site requests. To be able to
--   retrieve the data, I set up a php relay script which defines the CORS
--   header Access-Control-Allow-Origin. Various attempts to retrieve the data
--   from the original URL through the Http API failed; this way I was able to
--   demonstrate the correct workings of the application in handling the json
--   defined emails. The php relay script is included as "forward.php".


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed
-- Summary:
--   The task to retrieve the emails from the server has been set up to be
--   executed once every minute. The library "NoRedInk/elm-task-extra" was used
--   to easily set up the loop. The system was altered to prevent duplicate
--   Email instances from being added.


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

mapHotkeys : Bool -> Set KeyCode -> (Maybe Manager.Action)
mapHotkeys alt keyCodes =
  let is keyCode = Set.member keyCode keyCodes
  in if alt then
    if is 74 then -- Key J
      Just <| MoveSel True
    else if is 75 then -- Key K
      Just <| MoveSel False
    else if is 79 then -- Key O
      Just ToggleTrunc
    else if is 80 then -- Key P
      Just TogglePinned
    else if is 88 then -- Key X
      Just ToggleDone
    else if is 71 then -- Key G
      Just ToggleDoneVisibility
    else if is 65 then -- Key A
      Just ToggleAddVisibility
    else if is 72 then -- Key H
      Just ToggleSnoozerVisibility
    else
      Nothing
  else
    Nothing

mapReverseHotkey : Bool -> Set KeyCode -> (Maybe Manager.Action)
mapReverseHotkey alt keyCodes =
  let is keyCode = Set.member keyCode keyCodes
  in Just <| Reverse <| alt && (is 83) -- Key S

hotkeys : Signal (Maybe Manager.Action)
hotkeys =
  Signal.mergeMany
    [ Signal.map2 mapHotkeys Keyboard.alt Keyboard.keysDown
    , Signal.sampleOn (Time.fps 10)
      (Signal.map2 mapReverseHotkey Keyboard.alt Keyboard.keysDown)
    ]

mailbox : Signal.Mailbox (Maybe Manager.Action)
mailbox = Signal.mailbox Nothing

state : Signal Manager.Model
state =
  let update action model =
    case action of
      Just a -> Manager.update a model
      _ -> model
  in Signal.foldp update data (Signal.merge mailbox.signal hotkeys)

port runner : Task Http.Error ()
port runner =
  Task.Extra.loop Time.minute <|
    MailFetcher.fetch `Task.andThen`
      (AddMails >> Just >> Signal.send mailbox.address)

main : Signal Html
main =
  let view = Manager.view (Signal.forwardTo mailbox.address Just)
  in Signal.map view state
