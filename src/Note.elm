module Note exposing
    ( ClientOnlyNote
    , Note(..)
    , StoredNote
    , exampleNotes
    , insertNewNote
    , insertNotes
    , newNote
    , noteIdString
    , noteToPair
    , noteView
    , notesDecoder
    , patchNote
    , updateNoteText
    )

import AutoTextarea
import Html.Styled exposing (Html)
import Http
import Identifiers exposing (NotebookId, notebookIdToString, wordGenerator)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random exposing (Seed)
import Supabase exposing (getSupabase, patchSupabase, postSupabase, singletonDecoder, upsertSupabase)
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


encodeStoredNote : NotebookId -> StoredNote -> Value
encodeStoredNote notebookId (StoredNote noteId content) =
    Encode.object
        [ ( "id", Encode.int noteId )
        , ( "content", Encode.string content )
        , ( "notebook_id", Encode.string (notebookIdToString notebookId) )
        ]


encodeNote : NotebookId -> Note -> Value
encodeNote notebookId note =
    case note of
        ClientOnly n ->
            encodeClientNote notebookId n

        Stored n ->
            encodeStoredNote notebookId n


encodeNoteList : NotebookId -> List Note -> Value
encodeNoteList notebookId notes =
    Encode.list (encodeNote notebookId) notes


encodeClientNoteList : NotebookId -> List ClientOnlyNote -> Value
encodeClientNoteList notebookId notes =
    Encode.list (encodeClientNote notebookId) notes


noteDecoder : Decoder StoredNote
noteDecoder =
    Decode.map2
        StoredNote
        (Decode.field "id" Decode.int)
        (Decode.field "content" Decode.string)


notesDecoder : Decoder (List StoredNote)
notesDecoder =
    Decode.list noteDecoder


firstNoteDecoder : Decoder StoredNote
firstNoteDecoder =
    singletonDecoder noteDecoder


notesEndpoint : String
notesEndpoint =
    "notes"


noteEndpoint : NotebookId -> StoredNote -> String
noteEndpoint notebookId (StoredNote noteId content) =
    notesEndpoint ++ "?id=eq." ++ String.fromInt noteId ++ "&notebook_id=eq." ++ notebookIdToString notebookId


insertNotes : (Result Http.Error (List StoredNote) -> msg) -> NotebookId -> List ClientOnlyNote -> Cmd msg
insertNotes toMsg notebookId notes =
    postSupabase
        { path = notesEndpoint
        , body = encodeClientNoteList notebookId notes
        , decoder = notesDecoder
        , toMsg = toMsg
        }


insertNewNote : (Result Http.Error StoredNote -> msg) -> NotebookId -> Cmd msg
insertNewNote toMsg notebookId =
    postSupabase
        { path = notesEndpoint
        , body = encodeClientNote notebookId (ClientOnlyNote "" "")
        , decoder = firstNoteDecoder
        , toMsg = toMsg
        }


patchNote : (Result Http.Error StoredNote -> msg) -> NotebookId -> StoredNote -> Cmd msg
patchNote toMsg notebookId note =
    patchSupabase
        { path = noteEndpoint notebookId note
        , body = encodeStoredNote notebookId note
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
