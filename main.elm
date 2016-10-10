import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (..)

main = App.beginnerProgram
  { model = model
  , update = update
  , view = view
  }

--MODEL
type alias Model =
  { todo: String
  , todos: List String
  }

model =
  { todo = ""
  , todos = []
  }

--UPDATE
type Msg
  = UpdateText String
  | AddTodo
  | RemoveTodo String
update msg model =
  case msg of
    UpdateText text ->
      { model | todo = text }
    AddTodo ->
      { model
        | todos = model.todo :: model.todos
        , todo = "" }
    RemoveTodo text ->
      {model | todos = List.filter (\t -> t /= text) model.todos}

--VIEW
todoItem todo =
  li []
    [ text todo
    , button [onClick RemoveTodo] [text "X"]]
todoList todos =
  let
    children = List.map todoItem todos
  in
    ul [] children

view model =
  div []
    [ input [ type' "text"
            , onInput UpdateText
            , value model.todo] []
    , button [onClick AddTodo] [text "add todo item"]
    , todoList model.todos
    ]
