module Manager where

import Date exposing ( Date )
import Html exposing ( Attribute, Html )
import Html.Events as E
import Html.Attributes as A
import Json.Decode as Json

import ListItem
import MailItem
import Static exposing ( Email, Reminder )
import TodoItem

type alias ID = Int
type ItemModel = Mail MailItem.Model | Todo TodoItem.Model
type alias Model =
  { items: List (ID, ItemModel)
  , curId: ID
  , selected: Int
  , reminder: { date: String, body: String }
  , reversed: Bool
  , doneVisible: Bool
  }

type Action
  = MailAction ID MailItem.Action
  | TodoAction ID TodoItem.Action
  | AddReminder
  | EditReminderDate String
  | EditReminderBody String
  | MoveSel Bool
  | Reverse Bool
  | ToggleTrunc
  | TogglePinned
  | ToggleDone
  | ToggleDoneVisibility

init : Model
init =
  { items = []
  , curId = 0
  , selected = 0
  , reminder = { date = "2015-01-01", body = "" }
  , reversed = False
  , doneVisible = True
  }

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

modelOrder : (ID,  ItemModel) -> (ID, ItemModel) -> Order
modelOrder (id1, im1) (id2, im2) =
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
    else
      let
        t1 = Date.toTime li1.date
        t2 = Date.toTime li2.date
      in if t1 == t2 then EQ else if t1 < t2 then LT else GT

modelOrderReversed : (ID,  ItemModel) -> (ID, ItemModel) -> Order
modelOrderReversed (id1, im1) (id2, im2) =
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
    else
      let
        t1 = Date.toTime li1.date
        t2 = Date.toTime li2.date
      in if t1 == t2 then EQ else if t1 > t2 then LT else GT

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
        if li.done /= done then
          getDone done (List.drop 1 list)
        else
          (id, im) :: (getDone done (List.drop 1 list))

getSelected : Model -> Maybe (ID, ItemModel)
getSelected model =
  List.head <| List.drop model.selected model.items

-- UPDATE

updSelection : Int -> Int -> List (ID, ItemModel) -> List (ID, ItemModel)
updSelection sel idx list =
  case List.head list of
    Nothing -> []
    Just (id, im) ->
      let
        next = updSelection sel (idx + 1) (List.drop 1 list)
        action = ListItem.SetSelected (sel == idx)
      in
        case im of
          Mail mm ->
            (id, Mail <| MailItem.update (MailItem.LIAction action) mm) :: next
          Todo mm ->
            (id, Todo <| TodoItem.update (TodoItem.LIAction action) mm) :: next

sortModel : Model -> Model
sortModel model =
  if model.reversed then
    { model | items = List.sortWith modelOrderReversed model.items }
  else
    { model | items = List.sortWith modelOrder model.items }

updModel : Model -> Model
updModel model =
  let
    visibleItems =
      if model.doneVisible then
        List.length model.items
      else
        List.length (getDone False model.items)
    newSelected =
      if visibleItems == 0 then 0 else model.selected % visibleItems
    newModel =
      sortModel { model | selected = newSelected }
  in { newModel | items = updSelection newModel.selected 0 newModel.items }

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
      in updModel { model | items = List.map updateMI model.items }
    TodoAction id tiAction ->
      let updateTI (iid, model) =
        if id /= iid then
          (iid, model)
        else case model of
          Mail mModel -> (iid, model)
          Todo tModel -> (iid, Todo (TodoItem.update tiAction tModel))
      in updModel { model | items = List.map updateTI model.items }
    AddReminder ->
      let todo =
        { created = model.reminder.date
        , body = model.reminder.body
        }
      in let newModel = addTodos [todo] model
      in updModel { newModel | reminder = { date = "2015-01-01", body = "" } }
    EditReminderDate value ->
      let rm = model.reminder
      in updModel { model | reminder = { rm | date = value } }
    EditReminderBody value ->
      let rm = model.reminder
      in updModel { model | reminder = { rm | body = value } }
    MoveSel next ->
      if next then
        updModel { model | selected = model.selected + 1 }
      else
        updModel { model | selected = model.selected - 1 }
    Reverse rev ->
      updModel { model | reversed = rev }
    ToggleTrunc ->
      case getSelected model of
        Nothing -> model
        Just (id, im) -> update (MailAction id MailItem.Expand) model
    TogglePinned ->
      case getSelected model of
        Nothing -> model
        Just (id, im) ->
          let action = ListItem.Pin
          in case im of
            Mail mm -> update (MailAction id (MailItem.LIAction action)) model
            Todo mm -> update (TodoAction id (TodoItem.LIAction action)) model
    ToggleDone ->
      case getSelected model of
        Nothing -> model
        Just (id, im) ->
          let action = ListItem.MarkDone
          in case im of
            Mail mm -> update (MailAction id (MailItem.LIAction action)) model
            Todo mm -> update (TodoAction id (TodoItem.LIAction action)) model
    ToggleDoneVisibility ->
      updModel { model | doneVisible = not model.doneVisible }

-- VIEW

viewItem : Signal.Address Action -> (ID, ItemModel) -> Html
viewItem address (id, model) =
  case model of
    Mail mail -> MailItem.view (Signal.forwardTo address (MailAction id)) mail
    Todo reminder -> TodoItem.view (Signal.forwardTo address (TodoAction id)) reminder

onAddReminder : Signal.Address Action -> Action -> Attribute
onAddReminder address value =
  let options = E.defaultOptions
  in
    E.onWithOptions
      "submit"
      { options | preventDefault = True }
      Json.value
      (\_ -> Signal.message address value)

viewAddReminder : Signal.Address Action -> Model -> Html
viewAddReminder address model =
  Html.div [ A.class "panel panel-default" ]
    [ Html.div [ A.class "panel-heading" ]
      [ Html.h4 []
        [ Html.span [ A.class "glyphicon glyphicon-plus" ] []
        , Html.text "\160Add Reminder"
        ]
      ]
    , Html.div [ A.class "panel-body" ]
      [ Html.form [ onAddReminder address AddReminder ]
        [ Html.div [ A.class "form-group" ]
          [ Html.label [ A.attribute "for" "add-reminder-date" ]
            [ Html.text "Date" ]
          , Html.input
            [ A.attribute "type" "date"
            , A.class "form-control"
            , A.id "add-reminder-date"
            , A.name "add-reminder-date"
            , A.value model.reminder.date
            , A.placeholder "Date"
            , E.on "input" E.targetValue (Signal.message address << EditReminderDate)
            ] []
          ]
        , Html.div [ A.class "form-group" ]
          [ Html.label [ A.attribute "for" "add-reminder-body" ]
            [ Html.text "Message" ]
          , Html.textarea
            [ A.attribute "type" "text"
            , A.class "form-control"
            , A.id "add-reminder-body"
            , A.name "add-reminder-body"
            , A.value model.reminder.body
            , A.placeholder "Message"
            , A.style [ ("resize", "vertical") ]
            , E.on "input" E.targetValue (Signal.message address << EditReminderBody)
            ] []
          ]
        , Html.button
          [ A.attribute "type" "submit"
          , A.class "btn btn-default pull-right"
          ]
          [ Html.text "Add" ]
        ]
      ]
    ]

viewHotkeys : Html
viewHotkeys =
  Html.div [ A.class "panel panel-default" ]
    [ Html.div [ A.class "panel-heading" ]
      [ Html.h4 []
        [ Html.span [ A.class "glyphicon glyphicon-question-sign" ] []
        , Html.text "\160Hotkeys"
        ]
      ]
    , Html.div [ A.class "panel-body" ]
      [ Html.p []
        [ Html.span [ A.class "label label-default" ] [ Html.text "Alt" ]
        , Html.text "\160+\160"
        , Html.span [ A.class "label label-default" ] [ Html.text "J" ]
        , Html.text ": Select the next item."
        ]
      , Html.p []
        [ Html.span [ A.class "label label-default" ] [ Html.text "Alt" ]
        , Html.text "\160+\160"
        , Html.span [ A.class "label label-default" ] [ Html.text "K" ]
        , Html.text ": Select the previous item."
        ]
      , Html.p []
        [ Html.span [ A.class "label label-default" ] [ Html.text "Alt" ]
        , Html.text "\160+\160"
        , Html.span [ A.class "label label-default" ] [ Html.text "O" ]
        , Html.text ": Toggle truncation of the selected item."
        ]
      , Html.p []
        [ Html.span [ A.class "label label-default" ] [ Html.text "Alt" ]
        , Html.text "\160+\160"
        , Html.span [ A.class "label label-default" ] [ Html.text "P" ]
        , Html.text ": Toggle the \"Done\" status of the selected item."
        ]
      , Html.p []
        [ Html.span [ A.class "label label-default" ] [ Html.text "Alt" ]
        , Html.text "\160+\160"
        , Html.span [ A.class "label label-default" ] [ Html.text "X" ]
        , Html.text ": Toggle the \"Pinned\" status of the selected item."
        ]
      , Html.p []
        [ Html.span [ A.class "label label-default" ] [ Html.text "Alt" ]
        , Html.text "\160+\160"
        , Html.span [ A.class "label label-default" ] [ Html.text "S" ]
        , Html.text ": Hold to sort all items in reverse, ignoring the pinned status."
        ]
      , Html.p []
        [ Html.span [ A.class "label label-default" ] [ Html.text "Alt" ]
        , Html.text "\160+\160"
        , Html.span [ A.class "label label-default" ] [ Html.text "G" ]
        , Html.text ": Toggle the visibility of completed items."
        ]
      ]
    ]

viewItems : Signal.Address Action -> Model -> Html
viewItems address model =
  let
    completeItems = List.map (viewItem address) (getDone True model.items)
    incompleteItems = List.map (viewItem address) (getDone False model.items)
  in
    Html.div []
      (
        if (not model.doneVisible) || (List.isEmpty completeItems) then
          incompleteItems
        else if model.doneVisible && (List.isEmpty incompleteItems) then
          [ Html.h1 [] [ Html.text <| "Done" ] ] ++ completeItems
        else
          [ Html.h1 [] [ Html.text <| "To do" ] ] ++ incompleteItems ++
            [ Html.h1 [] [ Html.text <| "Done" ] ] ++ completeItems
      )

view : Signal.Address Action -> Model -> Html
view address model =
  let
    doneItems = getDone False model.items
  in
    Html.div
      [ A.class "container"
      , A.style [ ("padding-top", "20px") ]
      ]
      [ Html.div [ A.class "col-md-8" ]
        [ viewItems address model ]
      , Html.div [ A.class "col-md-4" ]
        [ viewAddReminder address model, viewHotkeys ]
      ]
