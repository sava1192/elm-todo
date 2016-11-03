import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Json.Decode

main : Program Never -- what does it mean?
main = App.beginnerProgram
  { model = model
  , update = update
  , view = view
  }

--MODEL
type alias Todo =
  { id: Int
  , content: String
  , checked: Bool
  }
type alias Model =
  { newTodoContent: String
  , todos: List Todo
  , index: Int
  }

model : Model
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
  | ChangeStatus Todo
  | NoOp
update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateText text ->
      { model | newTodoContent = text }
    AddTodo ->
        { model | todos
            = Todo model.index model.newTodoContent False :: model.todos
        , newTodoContent = ""
        , index = model.index + 1
        }
    RemoveTodo id ->
      { model | todos = List.filter (\t -> t.id /= id) model.todos }
    ChangeStatus todo ->
      { model | todos =
          List.map
            (\item ->
              if item.id == todo.id then
                Todo todo.id todo.content (not todo.checked)
              else
                item
            )
            model.todos
      }
    NoOp -> model

--VIEW
todoItem : Todo -> Html Msg
todoItem todo =
  li []
    [ input [ type' "checkbox"
            , checked todo.checked
            , onClick (ChangeStatus todo)] []
    , if todo.checked then
        node "strike" [] [text todo.content]
      else
        text todo.content
    , button [onClick (RemoveTodo todo.id)] [text "X"]]
todoList : List Todo -> Html Msg
todoList todos =
  let
    children = List.map todoItem todos
  in
    ul [] children

onKeyUp : Int -> Msg -> Attribute Msg
onKeyUp key msg =
  let
    mapper code =
      if code == key then
        msg
      else
        NoOp
  in
    on "keyup" (Json.Decode.map mapper keyCode)
view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text"
            , onInput UpdateText
            , onKeyUp 13 AddTodo
            , value model.newTodoContent] []
    , button [onClick AddTodo] [text "add todo item"]
    , todoList model.todos
    ]
