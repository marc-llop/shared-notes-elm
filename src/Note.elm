module Note exposing (ClientOnlyNote, Note(..), StoredNote, exampleNotes, insertNote, newNote, noteIdString, noteToPair, noteView, updateNoteText)

import AutoTextarea
import Html.Styled exposing (Html)
import Http
import Identifiers exposing (NotebookId, notebookIdToString, wordGenerator)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random exposing (Seed)
import Supabase exposing (postSupabase, singletonDecoder)
import Task exposing (Task)


type Note
    = ClientOnly ClientOnlyNote
    | Stored StoredNote


type ClientOnlyNote
    = ClientOnlyNote String String


type StoredNote
    = StoredNote Int String


noteIdString : Note -> String
noteIdString note =
    case note of
        ClientOnly (ClientOnlyNote id _) ->
            id

        Stored (StoredNote id _) ->
            String.fromInt id


newNote : Seed -> ( ClientOnlyNote, Seed )
newNote seed =
    let
        newNoteWithId : String -> ClientOnlyNote
        newNoteWithId noteId =
            ClientOnlyNote ("clientId-" ++ noteId) ""
    in
    Random.step wordGenerator seed
        |> Tuple.mapFirst newNoteWithId


updateNoteText : String -> Note -> Note
updateNoteText str note =
    case note of
        ClientOnly (ClientOnlyNote id _) ->
            ClientOnly (ClientOnlyNote id str)

        Stored (StoredNote id _) ->
            Stored (StoredNote id str)


encodeClientNote : NotebookId -> ClientOnlyNote -> Value
encodeClientNote notebookId (ClientOnlyNote _ content) =
    Encode.object
        [ ( "content", Encode.string content )
        , ( "notebook_id", Encode.string (notebookIdToString notebookId) )
        ]


encodeStoredNote : StoredNote -> Value
encodeStoredNote (StoredNote noteId content) =
    Encode.object
        [ ( "id", Encode.int noteId )
        , ( "content", Encode.string content )
        ]


noteDecoder : Decoder StoredNote
noteDecoder =
    Decode.map2
        StoredNote
        (Decode.field "id" Decode.int)
        (Decode.field "content" Decode.string)


firstNoteDecoder : Decoder StoredNote
firstNoteDecoder =
    singletonDecoder noteDecoder


endpoint : String
endpoint =
    "notes"


insertNote : (Result Http.Error StoredNote -> msg) -> NotebookId -> ClientOnlyNote -> Cmd msg
insertNote toMsg notebookId note =
    postSupabase
        { path = endpoint
        , body = encodeClientNote notebookId note
        , decoder = firstNoteDecoder
        , toMsg = toMsg
        }


noteToPair : Note -> ( String, Note )
noteToPair note =
    ( noteIdString note, note )


noteView : { note : Note, onInput : String -> String -> msg } -> ( String, Html msg )
noteView { note, onInput } =
    let
        noteId : String
        noteId =
            noteIdString note

        content : String
        content =
            case note of
                ClientOnly (ClientOnlyNote _ c) ->
                    c

                Stored (StoredNote _ c) ->
                    c
    in
    ( noteId
    , AutoTextarea.autoTextarea
        { value = content
        , onInput = onInput noteId
        , placeholder = ""
        }
    )


exampleNotes : List Note
exampleNotes =
    [ ClientOnlyNote "clientId-1" "Store model in localStorage as well so it can be started offline."
    , ClientOnlyNote "clientId-2" "Merge a notebook's notes in a non-destructive way whenever a content conflict is detected after downloading a note."
    , ClientOnlyNote "clientId-3" "Make notes automatically synchronized by using supabase client's real-time API on a JS port."
    , ClientOnlyNote "clientId-4" "Make note deletion undoable."
    , ClientOnlyNote "clientId-5" "Debounce database updates."
    , ClientOnlyNote "clientId-6" "Regenerate notebook ID upon primary key constraint violation on insert."
    ]
        |> List.map ClientOnly
