module Identifiers exposing (NotebookId, notebookIdToString, generateNotebookId, wordGenerator, parseNotebookId)

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
import Html.Attributes exposing (id)
import Regex

-- Identifies a Notebook. It renders as three dash-separated five-letter strings,
-- like dizzy-pacas-a6f87.
type NotebookId = NotebookId String

notebookIdFromWords : String -> String -> String -> NotebookId
notebookIdFromWords a b c = NotebookId <| String.join "-" [a, b, c]

notebookIdToString : NotebookId -> String
notebookIdToString (NotebookId a) = a

-- Length in characters of the shortIds that compose NotebookIds
idWordLength : Int
idWordLength = 5

wordSubRegex : String
wordSubRegex = "[a-z0-9]{" ++ String.fromInt idWordLength ++ "}"

notebookIdRegex : Regex.Regex
notebookIdRegex = 
    ["^", wordSubRegex, "-", wordSubRegex, "-", wordSubRegex, "$"]
        |> String.join ""
        |> Regex.fromString
        |> Maybe.withDefault Regex.never

-- Tries to parse a NotebookId. Valid NotebookIds are composed of three words
-- of 5 characters (a-z lowercase and 0-9) divided by "-".
parseNotebookId : String -> Result String NotebookId
parseNotebookId str = 
    if Regex.contains notebookIdRegex str
        then Ok (NotebookId str)
        else Err ("Tried to create an invalid notebookId: " ++ str)

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
        { url = "https://random-word-api.herokuapp.com/word?number=2&length=" ++ String.fromInt idWordLength
        , resolver = Http.Tasks.resolveJson wordsDecoder
        }

-- Auxiliary Error type that allows mixing (Task x a) with (Task Http.Error a)
type Error = Http Http.Error | None

{-| Generates a notebook ID of the form xxxxx-xxxxx-xxxxx (three words
of five lowercase alphanumeric characters separated by hyphens).
It attempts to get the first two words from an API that provides real
english words, so that the notebook ID is more readable, but if the
API fails it reverts to random characters.

    Task.perform (\s -> NotebookIdGenerated s) generateNotebookId

-}
generateNotebookId : Int -> Task x NotebookId
generateNotebookId randomSeed =
    let
        initialSeed : Seed
        initialSeed = Random.initialSeed randomSeed

        generateThreeShortIds : Seed -> Task x NotebookId
        generateThreeShortIds shortIds =
            generateNextShortId (notebookIdFromWords, initialSeed)
                |> Task.andThen (generateNextShortId)
                |> Task.andThen (generateNextShortId)
                |> Task.map (Tuple.first)

        appendGeneratedId : (String, String) -> Task Error NotebookId
        appendGeneratedId (a, b) = generateShortId initialSeed
            |> Task.map (\(c, _) -> notebookIdFromWords a b c)
            |> Task.mapError (\_ -> None)
    in
    -- Attempt to get two real words from an API, because it gives readable and nice IDs, and one random word.
    requestTwoWords
        |> Task.mapError Http
        |> Task.andThen appendGeneratedId
        -- If it fails, generate three random alphanumeric IDs and call it a day.
        |> Task.onError (\_ -> generateThreeShortIds initialSeed)


generateShortId : Seed -> Task x (String, Seed)
generateShortId randomSeed =
    Time.now
        |> Task.map (\_ -> Random.step wordGenerator randomSeed
            |> \(shortId, nextSeed) -> (shortId, nextSeed)
        )

generateNextShortId : (String -> a, Seed) -> Task x (a, Seed)
generateNextShortId (shortIds, randomSeed) =
    generateShortId randomSeed
        |> Task.map (\(generatedId, nextSeed) -> (shortIds generatedId, nextSeed))


wordGenerator : Generator String
wordGenerator =
    Random.String.string idWordLength alphanumericGenerator


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
