module Identifiers exposing (generateNotebookId, wordGenerator)

import Http
import Http.Tasks
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator)
import Random.Char
import Random.Extra
import Random.String
import Task exposing (Task)
import Time


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


requestTwoWords : Task Http.Error ( String, String )
requestTwoWords =
    Http.Tasks.get
        { url = "https://random-word-api.herokuapp.com/word?number=2&length=5"
        , resolver = Http.Tasks.resolveJson wordsDecoder
        }


{-| Generates a notebook ID of the form xxxxx-xxxxx-xxxxx (three words
of five lowercase alphanumeric characters separated by hyphens).
It attempts to get the first two words from an API that provides real
english words, so that the notebook ID is more readable, but if the
API fails it reverts to random characters.

    Task.perform (\s -> NotebookIdGenerated s) generateNotebookId

-}
generateNotebookId : Task x String
generateNotebookId =
    let
        generateTwoShortIds : a -> Task x ( String, String )
        generateTwoShortIds =
            \_ -> Task.map2 Tuple.pair generateShortId generateShortId
    in
    -- Attempt to get two real words from an API, because it gives readable and nice IDs.
    requestTwoWords
        -- If it fails, generate two random alphanum IDs and call it a day.
        |> Task.onError generateTwoShortIds
        |> Task.andThen (\( a, b ) -> generateShortId |> Task.map (\c -> [ a, b, c ]))
        |> Task.map (String.join "-")


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
generateShortId =
    randomToTask wordGenerator


wordGenerator : Generator String
wordGenerator =
    Random.String.string 5 alphanumericGenerator


alphanumericGenerator : Generator Char
alphanumericGenerator =
    Random.Extra.frequency
        ( 0.3, numericCharGenerator )
        [ ( 0.7, Random.Char.lowerCaseLatin ) ]


numericCharGenerator : Generator Char
numericCharGenerator =
    let
        intToChar : Int -> Char
        intToChar i =
            Char.fromCode (i + Char.toCode '0')
    in
    Random.int 0 9
        |> Random.map intToChar
