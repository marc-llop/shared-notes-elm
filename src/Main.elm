module Main exposing (main, Model, Msg)

import Browser
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class)
import Identifiers
import Task
-- import VitePluginHelper
import AutoTextarea exposing (autoTextarea)


type alias Note =
    ( String, String )


type alias Notes =
    List Note

type alias NotebookId = String

type Model
    = OpeningNewNotebook
    | NotebookOpen NotebookId Notes


exampleNotes : Notes
exampleNotes =
    [ ( "1", "Això és una nota" )
    , ( "2", "Això és una altra nota" )
    ]


init : String -> (Model, Cmd Msg)
init path = 
    let
        newNotebook : (Model, Cmd Msg)
        newNotebook = 
            ( OpeningNewNotebook
            , Task.perform IdGenerated Identifiers.generateNotebookId
            )

        existingNotebook : NotebookId -> (Model, Cmd Msg)
        existingNotebook notebookId = 
            ( NotebookOpen notebookId exampleNotes
            , Cmd.none
            )
    in
    case String.toList path of
        ['/'] -> newNotebook
        '/' :: notebookId -> existingNotebook (String.fromList notebookId)
        _ -> newNotebook


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = IdGenerated String
    | WriteNote String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IdGenerated notebookId ->
            ( NotebookOpen notebookId exampleNotes, Cmd.none )

        WriteNote _ -> (model, Cmd.none)


-- Styles defined in spinner.css
spinner : Html msg
spinner =
    div [ class "lds-ripple" ]
        [ div [] []
        , div [] []
        ]


openNotebook : NotebookId -> Notes -> Html Msg
openNotebook notebookId notes =
    span [ class "notebookId" ] [ text <| "Generated ID: " ++ notebookId ]

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
        [ div [ class "notebook" ]
            [ h1 [ class "title" ] [ text "Elm Shared Notes" ]
            , notebook
            , autoTextarea {value = "Hello there!", placeholder = "Write something...", onInput = WriteNote }
            ]
        ]
