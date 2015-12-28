module ListItem where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

type alias Model = { date: Date, pinned: Bool, done: Bool, selected: Bool }
type Action = Pin | MarkDone

init : Date -> Model
init iDate = { date = iDate, pinned = False, done = False, selected = False }

-- UPDATE

updSelection : Bool -> Model -> Model
updSelection sel model =
  { model | selected = sel }

update : Action -> Model -> Model
update action model =
  case action of
    Pin -> { model | pinned = not model.pinned }
    MarkDone -> { model | done = not model.done }

-- VIEW

view : Signal.Address Action -> Model -> List Html -> List Html -> List Html -> Html
view address model title subtitle content =
  Html.div [ A.class <| "panel panel-" ++ (if model.selected then "info" else "default") ]
    [ Html.div [ A.class "panel-heading" ]
      [ Html.div [ A.class "btn-group pull-right" ]
        [ Html.button
          [ A.class <| "btn btn-" ++ (if model.done then "success" else "default")
          , A.title (if model.done then "Undo" else "Mark as Done")
          , E.onClick address MarkDone
          ]
          [ Html.span [ A.class "glyphicon glyphicon-ok" ] [] ]
        , Html.button
          [ A.class <| "btn btn-" ++ (if model.pinned then "primary" else "default")
          , A.title "Pin"
          , E.onClick address Pin
          ]
          [ Html.span [ A.class "glyphicon glyphicon-pushpin" ] [] ]
        ]
      , Html.h4 [] title
      , Html.p [] subtitle
      ]
    , Html.div [ A.class "panel-body" ] content
    ]
