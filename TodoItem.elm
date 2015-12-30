module TodoItem where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import DateUtils
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

view : Signal.Address Action -> Model -> Html
view address model =
  let
    reminder = model.data
    expired =
      let
        resDLDate = Date.fromString reminder.deadline
      in
        case resDLDate of
          Err e -> False
          Ok cDLDate -> DateUtils.now > (Date.toTime cDLDate)
  in
    ListItem.view
      (Signal.forwardTo address LIAction)
      model.item
      ([ Html.span [ A.class "glyphicon glyphicon-bell" ] []
      , Html.text <| "\160" ++ reminder.created
      ] ++ (
        if expired then
          [ Html.text <| "\160"
          , Html.span [ A.class "label label-danger" ]
            [ Html.text "EXPIRED" ]
          ]
        else
          []
      ))
      [ Html.text <| "Deadline: " ++ reminder.deadline ]
      [ Html.text reminder.body ]
