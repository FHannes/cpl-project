module ListItem where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import DateUtils

type alias Model =
  { date: Date
  , pinned: Bool
  , done: Bool
  , selected: Bool
  , snoozed: (Maybe Date)
  }
type Action = Pin | MarkDone | SetSelected Bool | Snooze Date | Unsnooze

init : Date -> Model
init iDate =
  { date = iDate
  , pinned = False
  , done = False
  , selected = False
  , snoozed = Nothing
  }

isSnoozed : Model -> Bool
isSnoozed model =
  case model.snoozed of
    Nothing -> False
    Just date -> (Date.toTime date) > DateUtils.now

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Pin -> { model | pinned = not model.pinned }
    MarkDone -> { model | done = not model.done }
    SetSelected sel -> { model | selected = sel }
    Snooze date -> { model | snoozed = Just date }
    Unsnooze -> { model | snoozed = Nothing }

-- VIEW

view : Signal.Address Action -> Model -> List Html -> List Html -> List Html -> Html
view address model title subtitle content =
  Html.div [ A.class <| "panel panel-" ++ (if model.selected then "info" else "default") ]
    [ Html.div [ A.class "panel-heading" ]
      [ Html.div [ A.class "pull-right" ]
        [ Html.div [ A.class "btn-group" ]
          ((
            if isSnoozed model then
              [ Html.button
                [ A.class "btn btn-default"
                , A.title "Unsnooze"
                , E.onClick address Unsnooze
                ]
                [ Html.span [ A.class "glyphicon glyphicon-flash" ] [] ]
              ]
            else
              []
          ) ++
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
          ])
        ]
      , Html.h4 []
        (title ++ (
          if isSnoozed model then
            [ Html.text <| "\160"
            , Html.span [ A.class "label label-warning" ]
              [ Html.text "SNOOZED" ]
            ]
          else
            []
        ))
      , Html.p [] subtitle
      ]
    , Html.div [ A.class "panel-body" ] content
    ]
