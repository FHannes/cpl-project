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
  , selected: ID
  }

type Action = MailAction ID MailItem.Action | TodoAction ID TodoItem.Action

init : Model
init = { items = [], curId = 0, selected = 0 }

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

sortModel : (ID,  ItemModel) -> (ID, ItemModel) -> Order
sortModel (id1, im1) (id2, im2) =
  let
    li1 = (
      case im1 of
        Mail mm -> mm.item
        Todo mm -> mm.item
    )
    li2 = (
      case im2 of
        Mail mm -> mm.item
        Todo mm -> mm.item
    )
  in
    if li1.done /= li2.done then
      if li1.done == True then GT else LT
    else if li1.pinned /= li2.pinned then
      if li1.pinned == True then LT else GT
    else if (Date.toTime li1.date) < (Date.toTime li2.date) then LT else GT

getDone : Bool -> List (ID, ItemModel) -> List (ID, ItemModel)
getDone done list =
  case List.head list of
    Nothing -> []
    Just (id, im) ->
      let li = (
        case im of
          Mail mm -> mm.item
          Todo mm -> mm.item
      ) in
        if li.done == done then
          getDone done (List.drop 1 list)
        else
          (id, im) :: (getDone done (List.drop 1 list))

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

viewItem : Signal.Address Action -> ID -> (ID, ItemModel) -> Html
viewItem address selected (id, model) =
  case model of
    Mail mail -> MailItem.view (Signal.forwardTo address (MailAction id)) (id == selected) mail
    Todo reminder -> TodoItem.view (Signal.forwardTo address (TodoAction id)) (id == selected) reminder

view : Signal.Address Action -> Model -> Html
view address model =
  let
    sortedModel = {model | items = List.sortWith sortModel model.items }
    doneItems = getDone False sortedModel.items
  in
    Html.div [ A.class "container" ]
    ([ Html.h1 [] [ Html.text <| "To do" ] ] ++
    List.map (viewItem address model.selected) (getDone True sortedModel.items) ++
    (if not (List.isEmpty doneItems) then
      ([ Html.h1 [] [ Html.text "Done" ] ] ++
        List.map (viewItem address model.selected) doneItems)
    else
      [])
    )
