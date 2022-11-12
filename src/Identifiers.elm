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
import Random exposing (Seed)


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
generateNotebookId : Int -> Task x String
generateNotebookId randomSeed =
    let
        initialSeed : Seed
        initialSeed = Random.initialSeed randomSeed

        generateOneShortId : a -> Task x String
        generateOneShortId = \_ -> generateShortId initialSeed
            |> Task.map (Tuple.first)

        generateThreeShortIds : Seed -> Task x (List String)
        generateThreeShortIds shortIds =
            generateShortId initialSeed
                |> Task.map (Tuple.mapFirst One)
                |> Task.andThen (generateNextShortId)
                |> Task.andThen (generateNextShortId)
                |> Task.map (Tuple.first >> nonEmptyListToList)

        appendGeneratedId : (String, String) -> Task Error (List String)
        appendGeneratedId (a, b) = generateOneShortId ()
            |> Task.map (\c -> [ a, b, c ])
            |> Task.mapError (\_ -> None)
    in
    -- Attempt to get two real words from an API, because it gives readable and nice IDs, and one random word.
    requestTwoWords
        |> Task.mapError Http
        |> Task.andThen appendGeneratedId
        -- If it fails, generate three random alphanumeric IDs and call it a day.
        |> Task.onError (\_ -> generateThreeShortIds initialSeed)
        |> Task.map (String.join "-")

type Error = Http Http.Error | None

type NonEmptyList = One String | Some String NonEmptyList

generateShortId : Seed -> Task x (String, Seed)
generateShortId randomSeed =
    Time.now
        |> Task.map (\_ -> Random.step wordGenerator randomSeed
            |> \(shortId, nextSeed) -> (shortId, nextSeed)
        )

generateNextShortId : (NonEmptyList, Seed) -> Task x (NonEmptyList, Seed)
generateNextShortId (shortIds, randomSeed) =
    generateShortId randomSeed
        |> Task.map (\(generatedId, nextSeed) -> case shortIds of
            One previousId -> (Some generatedId (One previousId), nextSeed)
            Some _ _ -> (Some generatedId shortIds, nextSeed)
        )

nonEmptyListToList : NonEmptyList -> List String
nonEmptyListToList list = case list of
    One x -> [x]
    Some x xs -> x :: (nonEmptyListToList xs)

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
