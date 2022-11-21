module Notebook exposing (insertNotebook)

import Http
import Identifiers exposing (NotebookId, notebookIdToString, parseNotebookId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Supabase exposing (postSupabase)
import Task exposing (Task)


endpoint : String
endpoint =
    "notebooks"


encodeNotebookId : NotebookId -> Value
encodeNotebookId notebookId =
    Encode.object
        [ ( "id", Encode.string (notebookIdToString notebookId) )
        ]


resultToDecoder : Result String a -> Decoder a
resultToDecoder res =
    case res of
        Ok val ->
            Decode.succeed val

        Err message ->
            Decode.fail message


maybeToDecoder : String -> Maybe a -> Decoder a
maybeToDecoder err maybeVal =
    case maybeVal of
        Just val ->
            Decode.succeed val

        Nothing ->
            Decode.fail err


{-| Decodes a Notebook object to its notebookId.

    decodeString notebookIdDecoder "{\"id\": \"hello\"}" == Ok "hello"

-}
notebookIdDecoder : Decoder NotebookId
notebookIdDecoder =
    Decode.field "id" Decode.string
        |> Decode.andThen (parseNotebookId >> resultToDecoder)


{-| Decodes a Notebook objects list to the notebookId of the first object.

    decodeString firstNotebookIdDecoder "[{\"id\": \"hello\"}]" == Ok "hello"

    decodeString firstNotebookIdDecoder "[]" == Err "Unexpected empty response"

-}
firstNotebookIdDecoder : Decoder NotebookId
firstNotebookIdDecoder =
    Decode.list notebookIdDecoder
        |> Decode.andThen (List.head >> maybeToDecoder "Unexpected empty response")


insertNotebook : NotebookId -> Task Http.Error NotebookId
insertNotebook notebookId =
    postSupabase
        { path = endpoint
        , body = encodeNotebookId notebookId
        , decoder = notebookIdDecoder
        }
