module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODELS


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


type alias TodoList =
    { todos : List Todo
    , name : String
    }


type alias Model =
    { todoLists : List TodoList
    }



-- Msg


type Msg
    = AddTodoList



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodoList ->
            ( addNewTodoList model "Todo List 1", Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddTodoList ] [ text "Add New TodoList" ]
        , div [] (List.map renderTodoList model.todoLists)
        ]



-- FUNCS


renderTodoList : TodoList -> Html Msg
renderTodoList todoList =
    div []
        [ h2 [] [ text todoList.name ]
        , input [ type_ "text", placeholder "Add Your Todo"] []
        , ul [] (List.map renderTodo todoList.todos)
        ]


renderTodo : Todo -> Html Msg
renderTodo todo =
    li []
        [ text todo.title
        ]


addNewTodoList : Model -> String -> Model
addNewTodoList model name =
    let
        newToDoLists =
            (TodoList [] name) :: model.todoLists
    in
        ({ model | todoLists = newToDoLists })



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none )
