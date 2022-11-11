module Identifiers exposing (..)

import Http
import Http.Tasks
import Json.Decode as Decode exposing (Decoder)


wordsDecoder : Decoder ( String, String )
wordsDecoder =
    Decode.list Decode.string
        |> Decode.andThen
            (\words ->
                case words of
                    x :: y :: _ ->
                        Decode.succeed ( x, y )

                    _ ->
                        Decode.fail "Expected two words from random-word-api but received less."
            )


requestTwoWords =
    Http.Tasks.get
        { url = "https://random-word-api.herokuapp.com/word?number=2&length=5"
        , resolver = Http.Tasks.resolveJson wordsDecoder
        }
