module Notebook exposing (checkNotebookExists, getNotebookNotes, insertNotebook)

import Identifiers exposing (NotebookId, notebookIdToString, parseNotebookId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Note exposing (Note, storedNotesDecoder)
import Random exposing (Seed)
import Supabase exposing (CallError, getSupabase, postSupabase, singletonDecoder)


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
insertNotebook : (Result CallError NotebookId -> msg) -> NotebookId -> Cmd msg
insertNotebook toMsg notebookId =
    postSupabase
        { path = endpoint
        , body = encodeNotebookId notebookId
        , decoder = notebookIdDecoder
        , toMsg = toMsg
        }


{-| Returns the notebookId from the remote database if the notebook exists in it.
-}
checkNotebookExists : (Result CallError NotebookId -> msg) -> NotebookId -> Cmd msg
checkNotebookExists toMsg notebookId =
    getSupabase
        { path = endpoint ++ "?id=eq." ++ notebookIdToString notebookId
        , decoder = firstNotebookIdDecoder
        , toMsg = toMsg
        }


notebookNotesDecoder : Seed -> Decoder ( List Note, Seed )
notebookNotesDecoder seed =
    singletonDecoder (Decode.field "notes" (storedNotesDecoder seed))


getNotebookNotes : Seed -> (Result CallError ( List Note, Seed ) -> msg) -> NotebookId -> Cmd msg
getNotebookNotes seed toMsg notebookId =
    getSupabase
        { path = endpoint ++ "?id=eq." ++ notebookIdToString notebookId ++ "&select=notes(*)"
        , decoder = notebookNotesDecoder seed
        , toMsg = toMsg
        }
