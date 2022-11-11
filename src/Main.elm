module Main exposing (main)

import Browser
import HelloWorld exposing (helloWorld)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Task
import VitePluginHelper


type alias Model =
    Maybe ( String, String )


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
    | IdGenerated (Result Http.Error ( String, String ))
    | ShortIdGenerated String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateId ->
            ( model, Task.perform ShortIdGenerated IdentifiersSpec.generateShortId )

        IdGenerated result ->
            ( case result of
                Err _ ->
                    model

                Ok ( a, b ) ->
                    Just ( a, b )
            , Cmd.none
            )

        ShortIdGenerated a -> (Just (a, a), Cmd.none)


view : Model -> Html Msg
view maybeId =
    let
        notebookId =
            case maybeId of
                Nothing ->
                    "loading..."

                Just ( a, b ) ->
                    a ++ "-" ++ b
    in
    div []
        [ img [ src <| VitePluginHelper.asset "/src/assets/logo.png?inline", style "width" "200px" ] []
        , helloWorld
        , text <| "Generated ID: " ++ notebookId
        , button [ onClick GenerateId ] [ text "Generate ID" ]
        ]
