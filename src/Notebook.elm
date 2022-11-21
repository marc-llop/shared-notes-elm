module Notebook exposing (insertNotebook)

import Http
import Identifiers exposing (NotebookId, notebookIdToString, parseNotebookId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Supabase exposing (postSupabase, singletonDecoder)
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


{-| Decodes a Notebook object to its notebookId.

    decodeString notebookIdDecoder "{\"id\": \"hello\"}" == Ok "hello"

-}
notebookIdDecoder : Decoder NotebookId
notebookIdDecoder =
    Decode.field "id" Decode.string
        |> Decode.andThen (parseNotebookId >> resultToDecoder)


{-| Decodes a Notebook objects list to the notebookId of the first object.

    decodeString firstNotebookIdDecoder "[{\"id\": \"hello\"}]" == Ok "hello"

    decodeString firstNotebookIdDecoder "[]" == Err "Unexpected empty list"

-}
firstNotebookIdDecoder : Decoder NotebookId
firstNotebookIdDecoder =
    singletonDecoder notebookIdDecoder


{-| Inserts a new notebook with this ID in a remote database.
Fails if a notebook with this ID already exists.
-}
insertNotebook : NotebookId -> Task Http.Error NotebookId
insertNotebook notebookId =
    postSupabase
        { path = endpoint
        , body = encodeNotebookId notebookId
        , decoder = notebookIdDecoder
        }
