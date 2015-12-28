module MailItem where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import String

import ListItem
import Static exposing ( Email )

type alias Model = { data: Email, expanded: Bool, item: ListItem.Model }
type Action = Expand | LIAction ListItem.Action

init : Email -> Model
init mail =
  let date = Result.withDefault (Date.fromTime 0) (Date.fromString mail.date)
  in { data = mail, expanded = False, item = ListItem.init date }

-- UPDATE

updSelection : Bool -> Model -> Model
updSelection sel model =
  {model | item = ListItem.updSelection sel model.item }

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
    model.item
    [ Html.span [ A.class "glyphicon glyphicon-envelope" ] []
    , Html.text <| "\160" ++ mail.date ++ "\160| " ++ mail.title
    ]
    [ Html.text <| "From: " ++ mail.from ]
    (if (String.length mail.body) <= 200 then
      [ Html.text mail.body ]
    else
      [ Html.text (if model.expanded then mail.body else (String.slice 0 200 mail.body) ++ "...")
      , Html.br [] []
      , Html.button
        [ A.class "btn btn-default"
        , E.onClick address Expand
        ]
        [ Html.span [ A.class <| "glyphicon glyphicon-chevron-" ++
          (if model.expanded then "up" else "down") ] []
        , Html.text <| "\160" ++ (if model.expanded then "Less" else "More")
        ]
      ]
    )
