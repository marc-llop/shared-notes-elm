port module Main exposing (Flags, Model, Msg, main)

-- import VitePluginHelper

import AutoTextarea exposing (autoTextarea)
import Browser
import ButtonView exposing (buttonLinkView, buttonView)
import Css
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Html, div, h1, span, text, toUnstyled)
import Html.Styled.Attributes as Attributes exposing (class)
import Icons
import Identifiers exposing (NotebookId, notebookIdToString, parseNotebookId)
import Spinner exposing (spinner)
import Task


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


type alias Flags =
    { path : String, randomSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init { path, randomSeed } =
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
        , view = \model -> view model |> Html.Styled.toUnstyled
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

        AddNote ->
            ( model, Cmd.none )


noteView : { note : Note, onInput : String -> String -> msg } -> Html msg
noteView { note, onInput } =
    autoTextarea
        { value = Tuple.second note
        , onInput = onInput (Tuple.first note)
        , placeholder = ""
        }


buttonRow : Html msg
buttonRow =
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            ]
        ]
        [ buttonLinkView
            { icon = Icons.github
            , href = "https://github.com/marc-llop/shared-notes-elm"
            , description = "GitHub"
            }
        ]


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
        [ span [ class "notebookId" ] [ text <| "Notebook " ++ notebookIdToString notebookId ]
        , buttonRow
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
