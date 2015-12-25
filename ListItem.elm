module ListItem where

import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

-- UPDATE

-- VIEW
view : Html -> Html -> Html
view header content =
  Html.div [ A.class "panel panel-default" ]
    [ Html.div [ A.class "panel-heading" ]
      [ Html.div [ A.class "btn-group pull-right" ]
        [ Html.button [ A.class "btn btn-default", A.title "Mark as Done" ]
          [ Html.span [ A.class "glyphicon glyphicon-ok" ] [] ]
        , Html.button [ A.class "btn btn-default", A.title "Pin" ]
          [ Html.span [ A.class "glyphicon glyphicon-pushpin" ] [] ]
        ]
      , header
      ]
    , Html.div[ A.class "panel-body" ] [ content ]
    ]
