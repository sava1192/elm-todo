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
type alias Todo =
  { id: Int
  , content: String
  }
type alias Model =
  { newTodoContent: String
  , todos: List Todo
  , index: Int
  }

model =
  { newTodoContent = ""
  , todos = []
  , index = 0
  }

--UPDATE
type Msg
  = UpdateText String
  | AddTodo
  | RemoveTodo Int
update msg model =
  case msg of
    UpdateText text ->
      { model | newTodoContent = text }
    AddTodo ->
        { model | todos
            = Todo model.index model.newTodoContent :: model.todos
        , newTodoContent = ""
        , index = model.index + 1
        }
    RemoveTodo id ->
      {model | todos = List.filter (\t -> t.id /= id) model.todos}

--VIEW
todoItem todo =
  li []
    [ text todo.content
    , button [onClick (RemoveTodo todo.id)] [text "X"]]
todoList todos =
  let
    children = List.map todoItem todos
  in
    ul [] children

view model =
  div []
    [ input [ type' "text"
            , onInput UpdateText
            , value model.newTodoContent] []
    , button [onClick AddTodo] [text "add todo item"]
    , todoList model.todos
    ]
