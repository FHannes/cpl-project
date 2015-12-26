module Manager where

import Date exposing ( Date )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import MailItem
import Static exposing ( Email, Reminder )
import TodoItem

type alias ID = Int
type ItemModel = Mail MailItem.Model | Todo TodoItem.Model
type alias Model =
  { items: List (ID, ItemModel)
  , curId: ID
  }

type Action = MailAction ID MailItem.Action | TodoAction ID TodoItem.Action

init : Model
init = { items = [], curId = 0 }

addMails : List Email -> Model -> Model
addMails mails model =
  case List.head mails of
    Nothing -> model
    Just mail ->
      let newModel = addMails (List.drop 1 mails) { model | curId = model.curId + 1 }
      in { newModel | items = ( model.curId, Mail <| MailItem.init mail) :: newModel.items }

addTodos : List Reminder -> Model -> Model
addTodos reminders model =
  case List.head reminders of
    Nothing -> model
    Just reminder ->
      let newModel = addTodos (List.drop 1 reminders) { model | curId = model.curId + 1 }
      in { newModel | items = ( model.curId, Todo <| TodoItem.init reminder) :: newModel.items }

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    MailAction id miAction ->
      let updateMI (iid, model) =
        if id /= iid then
          (iid, model)
        else case model of
          Todo tModel -> (iid, model)
          Mail mModel -> (iid, Mail (MailItem.update miAction mModel))
      in { model | items = List.map updateMI model.items }
    TodoAction id tiAction ->
      let updateTI (iid, model) =
        if id /= iid then
          (iid, model)
        else case model of
          Mail mModel -> (iid, model)
          Todo tModel -> (iid, Todo (TodoItem.update tiAction tModel))
      in { model | items = List.map updateTI model.items }

-- VIEW

viewItem : Signal.Address Action -> (ID, ItemModel) -> Html
viewItem address (id, model) =
  case model of
    Mail mail -> MailItem.view (Signal.forwardTo address (MailAction id)) mail
    Todo reminder -> TodoItem.view (Signal.forwardTo address (TodoAction id)) reminder

view : Signal.Address Action -> Model -> Html
view address model = Html.div [ A.class "container" ]
  ([ Html.h1 [] [ Html.text "To do" ] ] ++
  List.map (viewItem address) model.items ++
  [ Html.h1 [] [ Html.text "Done" ] ])
