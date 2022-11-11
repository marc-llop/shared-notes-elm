module Main exposing (main)

import Browser
import HelloWorld exposing (helloWorld)
import Html exposing (Html, div, img, button, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import VitePluginHelper


main : Program () Int Msg
main =
    Browser.sandbox {
        init = 0,
        update = update,
        view = view
    }

type Msg
    = GenerateId

update : Msg -> number -> number
update msg model =
    case msg of
        GenerateId ->
            model + 1


view : Int -> Html Msg
view model =
    div []
        [ img [ src <| VitePluginHelper.asset "/src/assets/logo.png?inline", style "width" "200px" ] []
        , helloWorld model
        , text <| "Count is: " ++ String.fromInt model
        , button [ onClick GenerateId ] [ text "Generate ID" ]
        ]
