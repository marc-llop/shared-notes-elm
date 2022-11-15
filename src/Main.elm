port module Main exposing (Model, Msg, main)

-- import VitePluginHelper

import AutoTextarea exposing (autoTextarea)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)
import Identifiers exposing (NotebookId, parseNotebookId, notebookIdToString)
import Task
import ButtonView exposing (buttonView)
import Icons
import Json.Decode as Decode
import Regex

port updateLocation : String -> Cmd msg


type alias Note =
    ( String, String )


type alias Notes =
    Dict String String


type Model
    = OpeningNewNotebook
    | NotebookOpen NotebookId Notes


exampleNotes : Notes
exampleNotes =
    [ ( "1", "Això és una nota" )
    , ( "2", "Això és una altra nota" )
    , ( "3", "Això és una altra nota" )
    , ( "4", "Això és una altra nota" )
    , ( "5", "Això és una altra nota" )
    , ( "6", "Això és una altra nota" )
    , ( "7", "Això és una altra nota" )
    , ( "8", "Això és una altra nota" )
    , ( "9", "Això és una altra nota" )
    , ( "21", "Això és una altra nota" )
    , ( "22", "Això és una altra nota" )
    , ( "23", "Això és una altra nota" )
    , ( "24", "Això és una altra nota" )
    , ( "25", "Això és una altra nota" )
    , ( "26", "Això és una altra nota" )
    , ( "27", "Això és una altra nota" )
    , ( "28", "Nota final" )
    ]
        |> Dict.fromList

type alias Flags = {path : String, randomSeed: Int}

init : Flags -> ( Model, Cmd Msg )
init {path, randomSeed} =
    let
        newNotebook : ( Model, Cmd Msg )
        newNotebook =
            ( OpeningNewNotebook
            , Task.perform IdGenerated (Identifiers.generateNotebookId randomSeed)
            )

        existingNotebook : NotebookId -> ( Model, Cmd Msg )
        existingNotebook notebookId =
            ( NotebookOpen notebookId exampleNotes
            , Cmd.none
            )
    in
    case String.toList path of
        [ '/' ] ->
            newNotebook

        '/' :: notebookId ->
            String.fromList notebookId
                |> parseNotebookId
                |> Result.map existingNotebook
                |> Result.withDefault newNotebook

        _ ->
            newNotebook


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = IdGenerated NotebookId
    | WriteNote String String
    | AddNote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IdGenerated notebookId ->
            ( NotebookOpen notebookId exampleNotes, updateLocation (notebookIdToString notebookId) )

        WriteNote noteId value ->
            ( case model of
                OpeningNewNotebook ->
                    model

                NotebookOpen notebookId notes ->
                    NotebookOpen notebookId
                        (Dict.update noteId (\_ -> Just value) notes)
            , Cmd.none
            )

        AddNote -> (model, Cmd.none)



-- Styles defined in spinner.css


spinner : Html msg
spinner =
    div [ class "lds-ripple" ]
        [ div [] []
        , div [] []
        ]


noteView : { note : Note, onInput : String -> String -> msg } -> Html msg
noteView { note, onInput } =
    autoTextarea
        { value = Tuple.second note
        , onInput = onInput (Tuple.first note)
        , placeholder = ""
        }


openNotebook : NotebookId -> Notes -> Html Msg
openNotebook notebookId notes =
    let
        notesList : List (Html Msg)
        notesList =
            Dict.toList notes
                |> List.sortBy (Tuple.first >> String.toInt >> Maybe.withDefault 0)
                |> List.map (\note -> noteView { note = note, onInput = WriteNote })
    in
    div [ class "notebook" ]
        [ span [ class "notebookId" ] [ text <| "Generated ID: " ++ notebookIdToString notebookId ]
        , div [ class "notesList" ] notesList
        , buttonView { icon = Icons.plus, onClick = AddNote, description = "Add Note" }
        ]


view : Model -> Html Msg
view model =
    let
        notebook : Html Msg
        notebook =
            case model of
                OpeningNewNotebook ->
                    spinner

                NotebookOpen notebookId notes ->
                    openNotebook notebookId notes
    in
    div [ class "screen" ]
        [ div [ class "notebookScreen" ]
            [ h1 [ class "title" ] [ text "Elm Shared Notes" ]
            , notebook
            ]
        ]
