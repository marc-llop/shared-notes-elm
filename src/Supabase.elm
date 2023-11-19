module Supabase exposing (CallError(..), deleteSupabaseTask, getSupabase, patchSupabase, postSupabase, singletonDecoder, upsertSupabase)

import Http exposing (emptyBody, expectJson, header, jsonBody, request, task)
import Http.Tasks as HttpTasks
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Task exposing (Task)


{-| Error type that simplifies Http.Error in two variants of interest:

  - `DataError`: The request reached the server alright, but there is something
    wrong. Typically, this will be database errors like NotFound,
    AlreadyExists...
  - `ConnectionError`: There is no connection, the server didn't respond, or
    it had an internal crash. Whatever it is, the application can assume it is
    not currently possible to store the changes and continue in offline mode.

-}
type CallError
    = DataError
    | ConnectionError


httpErrorToCallError : Http.Error -> CallError
httpErrorToCallError httpError =
    case httpError of
        Http.BadStatus status ->
            if status < 500 then
                DataError

            else
                ConnectionError

        Http.BadBody _ ->
            DataError

        _ ->
            ConnectionError


httpResultToCallError : Result Http.Error a -> Result CallError a
httpResultToCallError =
    Result.mapError httpErrorToCallError


clientKey : String
clientKey =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImFrZ3dvZ3ppdHJvcXNrZG1nd3FqIiwicm9sZSI6ImFub24iLCJpYXQiOjE2Njg2MTg2MDksImV4cCI6MTk4NDE5NDYwOX0.9GQWV1W8wR5vZgIcgr1wSCIGOeC5poAgQdQLNX83bQI"


headers : List Http.Header
headers =
    [ header "apikey" clientKey
    ]


supabaseUrl : String
supabaseUrl =
    "https://akgwogzitroqskdmgwqj.supabase.co/rest/v1"


endpoint : String -> String
endpoint path =
    if String.startsWith "/" path then
        supabaseUrl ++ path

    else
        supabaseUrl ++ "/" ++ path


{-| Read rows of a table. Path should contain any
PostgREST filters you want applied.

    getSupabase
        { path = "notes?id=eq.1&select=some_column,other_table(foreign_key)"
        , decoder = noteListDecoder
        , toMsg = NotesFetched
        }

-}
getSupabase :
    { path : String
    , decoder : Decoder a
    , toMsg : Result CallError a -> msg
    }
    -> Cmd msg
getSupabase { path, decoder, toMsg } =
    request
        { method = "GET"
        , headers = headers
        , url = endpoint path
        , body = emptyBody
        , expect = expectJson (httpResultToCallError >> toMsg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Insert one or many rows in a table.

    postSupabase
        { path = "notes"
        , body = encodeNote note
        , decoder = noteDecoder
        , toMsg = NoteStored
        }

-}
postSupabase :
    { path : String
    , body : Value
    , decoder : Decoder a
    , toMsg : Result CallError a -> msg
    }
    -> Cmd msg
postSupabase { path, body, decoder, toMsg } =
    request
        { method = "POST"
        , headers = headers ++ [ header "Prefer" "return=representation" ]
        , url = endpoint path
        , body = jsonBody body
        , expect = expectJson (httpResultToCallError >> toMsg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Upsert several rows in a table (insert them
and in case of any collision by "id", update the
already existing rows).

    upsertSupabase
        { path = "notes"
        , body = encodeNoteList notes
        , decoder = nodeListDecoder
        , toMsg = NoteUpdated
        }

-}
upsertSupabase :
    { path : String
    , body : Value
    , decoder : Decoder a
    , toMsg : Result CallError a -> msg
    }
    -> Cmd msg
upsertSupabase { path, body, decoder, toMsg } =
    request
        { method = "POST"
        , headers =
            headers
                ++ [ header "Prefer" "resolution=merge-duplicates"
                   , header "Prefer" "return=representation"
                   ]
        , url = endpoint path
        , body = jsonBody body
        , expect = expectJson (httpResultToCallError >> toMsg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Updates all matching rows. You can filter affected rows with path.

    patchSupabase
        { path = "notes?id=eq.1"
        , body = encodeNote updatedNote
        , decoder = noteDecoder
        , toMsg = NoteUpdated
        }

-}
patchSupabase :
    { path : String
    , body : Value
    , decoder : Decoder a
    , toMsg : Result CallError a -> msg
    }
    -> Cmd msg
patchSupabase { path, body, decoder, toMsg } =
    request
        { method = "PATCH"
        , headers = headers ++ [ header "Prefer" "return=representation" ]
        , url = endpoint path
        , body = jsonBody body
        , expect = expectJson (httpResultToCallError >> toMsg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Deletes all matching rows. You can filter affected rows with path.

    deleteSupabase
        { path = "notes?id=eq.1"
        , toMsg = NoteUpdated
        }

-}
deleteSupabaseTask :
    { path : String
    }
    -> Task CallError ()
deleteSupabaseTask { path } =
    task
        { method = "DELETE"
        , headers = headers
        , url = endpoint path
        , body = emptyBody
        , resolver = HttpTasks.resolveWhatever
        , timeout = Nothing
        }
        |> Task.mapError httpErrorToCallError


maybeToDecoder : String -> Maybe a -> Decoder a
maybeToDecoder err maybeVal =
    case maybeVal of
        Just val ->
            Decode.succeed val

        Nothing ->
            Decode.fail err


{-| Given a Decoder x, returns a new decoder that
decodes a List x and returns only the first item,
and fails in case of empty list.

    decodeString (singletonDecoder Decode.int) "[1, 2]" == Ok 1

    decodeString (singletonDecoder Decode.int) "[]" == Err "Unexpected empty list"

-}
singletonDecoder : Decoder a -> Decoder a
singletonDecoder decoder =
    Decode.list decoder
        |> Decode.andThen (List.head >> maybeToDecoder "Unexpected empty list while decoding")
