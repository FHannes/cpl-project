module MailItem where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import ListItem
import Static exposing ( Email )

type alias Model = { data: Email, expanded: Bool, item: ListItem.Model }
type Action = Expand | LIAction ListItem.Action

init : Email -> Model
init mail =
  let date = Result.withDefault (Date.fromTime 0) (Date.fromString mail.date)
  in { data = mail, expanded = False, item = ListItem.init date }

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Expand -> { model | expanded = not model.expanded }
    LIAction liAction -> { model | item = ListItem.update liAction model.item }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let mail = model.data
  in ListItem.view
    (Signal.forwardTo address LIAction)
    (Html.div []
      [ Html.h4 []
        (if model.item.pinned then
          [ Html.span [ A.class "glyphicon glyphicon-envelope" ] []
          , Html.text <| "\160" ++ mail.date ++ "\160|\160" ++ mail.title
          , Html.span [ A.class "badge" ] [ Html.text "PINNED" ]
          ]
        else
          [ Html.span [ A.class "glyphicon glyphicon-envelope" ] []
          , Html.text <| "\160" ++ mail.date ++ "\160|\160" ++ mail.title
          ]
        )
      , Html.p [] [ Html.text <| "From: " ++ mail.from ]
      ])
    (Html.text mail.body)
