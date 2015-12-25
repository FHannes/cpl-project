module TodoItem where

import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import ListItem
import Static exposing ( Reminder )

-- UPDATE

-- VIEW
view : Reminder -> Html
view reminder =
  ListItem.view
    (Html.div []
      [ Html.h4 []
        [ Html.span [ A.class "glyphicon glyphicon-time" ] []
        , Html.text <| "\160" ++ reminder.created ++ " | TODO"
        ]
      ])
    (Html.text reminder.body)
