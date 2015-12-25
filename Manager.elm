module Manager where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import MailItem
import Static exposing ( Email, Reminder )
import TodoItem
import Utils

type ItemData = Mail Email | Todo Reminder
type alias Item =
  { data: ItemData
  , date: Date
  , pinned: Bool
  , expand: Bool
  , done: Bool
  }
type alias Model = List Item

type Action = MarkDone | Pin | Expand

init : Model
init = []

addMails : List Email -> Model -> Model
addMails mails model =
  if (List.isEmpty mails) then
    model
  else let mail = (Utils.getMaybe (List.head mails)) in
    { data = Mail mail
    , date = Utils.getDate <| Date.fromString mail.date
    , pinned = False
    , expand = False
    , done = False
    } :: addMails (List.drop 1 mails) model

addTodos : List Reminder -> Model -> Model
addTodos reminders model =
  if (List.isEmpty reminders) then
    model
  else let reminder = (Utils.getMaybe (List.head reminders)) in
    { data = Todo reminder
    , date = Utils.getDate <| Date.fromString reminder.created
    , pinned = False
    , expand = False
    , done = False
    } :: addTodos (List.drop 1 reminders) model

-- UPDATE

-- VIEW

viewItem : Item -> Html
viewItem item =
  case item.data of
    Mail mail -> MailItem.view mail
    Todo reminder -> TodoItem.view reminder

view : Model -> Html
view model = Html.div [ A.class "container" ]
  (List.concat
  [ [Html.h1 [] [ Html.text "To do" ]]
  , List.map viewItem model
  , [Html.h1 [] [ Html.text "Done" ]]
  ])
