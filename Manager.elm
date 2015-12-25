module Manager where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import MailItem
import Static exposing ( Email, Reminder )

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

addItem : ItemData -> Model -> Model
addItem item model =
  case item of
    Mail mail ->
      { data = item
      , date = Date.fromString mail.date
      , pinned = False
      , expand = False
      , done = False
      }
    Todo reminder ->
      { data = item
      , date = Date.fromString reminder.created
      , pinned = False
      , expand = False
      , done = False
      }

-- UPDATE

-- VIEW

viewItem : Item -> Html
viewItem item =
  case item.data of
    Mail mail -> MailItem.view mail
    Todo reminder -> Html.p [] [ Html.text "Test" ]

view : Model -> Html
view model = Html.div [ A.class "container" ]
  List.concat
  [ [Html.h1 [] [ Html.text "To do" ]]
  , (List.append <| List.map viewItem model)
  , [Html.h1 [] [ Html.text "Done" ]]
  ]
