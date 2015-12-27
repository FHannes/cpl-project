module TodoItem where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import ListItem
import Static exposing ( Reminder )

type alias Model = { data: Reminder, item: ListItem.Model }
type Action = LIAction ListItem.Action

init : Reminder -> Model
init reminder =
  let date = Result.withDefault (Date.fromTime 0)  (Date.fromString reminder.created)
  in { data = reminder, item = ListItem.init date }

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    LIAction liAction -> { model | item = ListItem.update liAction model.item }

-- VIEW

view : Signal.Address Action -> Bool -> Model -> Html
view address selected model =
  let reminder = model.data
  in ListItem.view
    (Signal.forwardTo address LIAction)
    selected
    model.item
    [ Html.span [ A.class "glyphicon glyphicon-time" ] []
    , Html.text <| "\160" ++ reminder.created
    ]
    []
    [ Html.text reminder.body ]
