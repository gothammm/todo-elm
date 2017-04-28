module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
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
    { id : Int
    , name : String
    , todos : List Todo
    }


type alias Model =
    { todoLists : List TodoList
    , todoTitle : String
    , nextTodoListId : TodoListId
    , nextTodoId : TodoId
    }

type alias TodoListId = Int
type alias TodoId = Int
-- Msg


type Msg
    = AddTodoList
    | AddTodo TodoListId
    | UpdateTitle String

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodoList ->
            ( addNewTodoList model ("Todo List " ++ toString(model.nextTodoListId)), Cmd.none )

        AddTodo todoListId ->
            ( addNewToDo model todoListId, Cmd.none )

        UpdateTitle title ->
            ({ model | todoTitle = title }, Cmd.none )


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
        , input [ type_ "text", placeholder "Add Your Todo", onInput UpdateTitle] []
        , button [onClick (AddTodo todoList.id)] [text "Add"]
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
            (TodoList model.nextTodoListId name []) :: model.todoLists
    in
        ({ model | todoLists = newToDoLists, nextTodoListId = model.nextTodoListId + 1 })

addNewToDo : Model -> TodoListId -> Model
addNewToDo model todoListId =
    let
        newToDo =
            Todo model.nextTodoId model.todoTitle False
        todoList =
            firstTodoList (filter (isIdEqual todoListId) model.todoLists)
        updatedTodoList = { todoList | todos = newToDo :: todoList.todos }

    in
        { model | todoLists = [updatedTodoList], nextTodoId = model.nextTodoId + 1 }

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
            TodoList 1 "New List" []

isIdEqual : TodoListId -> TodoList -> Bool
isIdEqual todoListId todoList =
    todoListId == todoList.id

-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model [] "" 1 1, Cmd.none )
