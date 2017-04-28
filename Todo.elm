module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)


-- import Html.Lazy exposing (lazy, lazy2)

import Json.Decode as Json


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
    , id : Int
    , savedText: String
    }


type alias Model =
    { todoLists : List TodoList
    , todoTitle : String
    }



-- Msg


type Msg
    = AddTodoList
    | AddTodo String
    | UpdateTitle String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodoList ->
            ( addNewTodoList model "Todo List ", Cmd.none )

        AddTodo content ->
            ( addNewToDo model, Cmd.none )

        UpdateTitle title ->
            ( { model | todoTitle = title }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddTodoList ] [ text "Add New TodoList" ]
        , div [] (List.map renderTodoList model.todoLists)
        ]



-- FUNCTIONS


renderTodoList : TodoList -> Html Msg
renderTodoList todoList =
    div []
        [ h2 [] [ text todoList.name ]
        , input [ type_ "text", placeholder "Add Your Todo", onEnter (AddTodo todoList.savedText), onInput UpdateTitle ] []
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
        todoItemNumber =
            (length model.todoLists) + 1

        newToDoLists =
            (TodoList [] (name ++ toString todoItemNumber) todoItemNumber) "" :: model.todoLists
    in
        ({ model | todoLists = newToDoLists })


addNewToDo : Model -> Model
addNewToDo model =
    let
        newToDo =
            Todo 1 model.todoTitle False

        todoList =
            firstTodoList model.todoLists

        updatedTodoList =
            { todoList | todos = newToDo :: todoList.todos }
    in
        { model | todoLists = [ updatedTodoList ] }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


firstTodoList : List TodoList -> TodoList
firstTodoList todoLists =
    case List.head todoLists of
        Just list ->
            list

        Nothing ->
            TodoList [] "New List" 1



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model [] "", Cmd.none )
