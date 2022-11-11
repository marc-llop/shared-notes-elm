module Identifiers exposing (..)

import Http
import Http.Tasks
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator)
import Task exposing (Task)
import Time
import Random
import Random.String
import Random.Char
import Random.Extra


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


randomToTask : Generator a -> Task x a
randomToTask generator =
    Time.now
        |> Task.map
            (\posix ->
                posix
                    |> Time.posixToMillis
                    |> Random.initialSeed
                    |> Random.step generator
                    |> Tuple.first
            )

generateShortId : Task x String
generateShortId = randomToTask wordGenerator

wordGenerator : Generator String
wordGenerator = Random.String.string 5 alphanumericGenerator

alphanumericGenerator : Generator Char
alphanumericGenerator =
    Random.Extra.frequency
        (0.3, numericCharGenerator)
        [ (0.7, Random.Char.lowerCaseLatin) ]

numericCharGenerator : Generator Char
numericCharGenerator =
    let
        intToChar i = Char.fromCode (i + Char.toCode '0')
    in
    Random.int 0 9
        |> Random.map intToChar