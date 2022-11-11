module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, span, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Http
import Identifiers
import Task
import VitePluginHelper


type alias Note =
    ( String, String )


type alias Notes =
    List Note


type Model
    = OpeningNewNotebook
    | NotebookOpen String Notes


exampleNotes : Notes
exampleNotes =
    [ ( "1", "Això és una nota" )
    , ( "2", "Això és una altra nota" )
    ]


init : String -> (Model, Cmd Msg)
init path = 
    let
        newNotebook = 
            ( OpeningNewNotebook
            , Task.perform IdGenerated Identifiers.generateNotebookId
            )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IdGenerated notebookId ->
            ( NotebookOpen notebookId exampleNotes, Cmd.none )


spinner : Html msg
spinner =
    div [ class "lds-ripple" ]
        [ div [] []
        , div [] []
        ]


view : Model -> Html Msg
view model =
    let
        notebook =
            case model of
                OpeningNewNotebook ->
                    spinner

                NotebookOpen notebookId _ ->
                    span [ class "notebookId" ] [ text <| "Generated ID: " ++ notebookId ]
    in
    div [ class "screen" ]
        [ div [ class "notebook" ]
            [ h1 [ class "title" ] [ text "Elm Shared Notes" ]
            , notebook
            ]
        ]
