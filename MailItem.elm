module MailItem where

import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import ListItem
import Static exposing ( Email )

-- UPDATE

-- VIEW
view : Email -> Html
view mail =
  ListItem.view
    (Html.div []
      [ Html.h4 []
        [ Html.span [ A.class "glyphicon glyphicon-envelope" ] []
        , Html.text <| "\160" ++ mail.date ++ "\160|\160" ++ mail.title
        ]
      , Html.p [] [ Html.text <| "From: " ++ mail.from ]
      ])
    (Html.text mail.body)
