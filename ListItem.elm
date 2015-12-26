module ListItem where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

type alias Model = { date: Date, pinned: Bool, done: Bool }
type Action = Pin | MarkDone

init : Date -> Model
init iDate = { date = iDate, pinned = False, done = False }

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Pin -> { model | pinned = not model.pinned }
    MarkDone -> { model | done = not model.done }

-- VIEW

view : Signal.Address Action -> Html -> Html -> Html
view address header content =
  Html.div [ A.class "panel panel-default" ]
    [ Html.div [ A.class "panel-heading" ]
      [ Html.div [ A.class "btn-group pull-right" ]
        [ Html.button
          [ A.class "btn btn-default"
          , A.title "Mark as Done"
          , E.onClick address Pin
          ]
          [ Html.span [ A.class "glyphicon glyphicon-ok" ] [] ]
        , Html.button
          [ A.class "btn btn-default"
          , A.title "Pin"
          , E.onClick address MarkDone
          ]
          [ Html.span [ A.class "glyphicon glyphicon-pushpin" ] [] ]
        ]
      , header
      ]
    , Html.div [ A.class "panel-body" ] [ content ]
    ]
