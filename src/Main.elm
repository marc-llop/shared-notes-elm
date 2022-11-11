module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, text, span, h1)
import Html.Attributes exposing (src, style, class)
import Html.Events exposing (onClick)
import Http
import Identifiers
import Task
import VitePluginHelper


type alias Model =
    Maybe String


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Nothing, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = GenerateId
    | IdGenerated String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateId ->
            ( model, Task.perform IdGenerated Identifiers.generateNotebookId )

        IdGenerated id ->
            ( Just id, Cmd.none )



view : Model -> Html Msg
view maybeId =
    let
        notebookId = Maybe.withDefault "Generate an ID!" maybeId
    in
    div [ class "screen"]
        [ div [ class "notebook" ]
            [ h1 [ class "title" ] [ text "Elm Shared Notes" ]
            , span [ class "notebookId" ] [ text <| "Generated ID: " ++ notebookId ]
            , button [ onClick GenerateId ] [ text "Generate ID" ]
            ]
        ]
