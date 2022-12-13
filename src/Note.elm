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
    , patchNote
    , storedNotesDecoder
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
    = ClientOnlyNote ClientId String


type StoredNote
    = StoredNote ServerId ClientId String


type alias ClientId =
    String


type alias ServerId =
    Int


noteIdString : Note -> String
noteIdString note =
    case note of
        ClientOnly (ClientOnlyNote clientId _) ->
            clientId

        Stored (StoredNote _ clientId _) ->
            clientId


newClientId : Seed -> ( ClientId, Seed )
newClientId seed =
    Random.step wordGenerator seed
        |> Tuple.mapFirst (\noteId -> "clientId-" ++ noteId)


newNote : Seed -> ( ClientOnlyNote, Seed )
newNote seed =
    let
        newNoteWithId : ClientId -> ClientOnlyNote
        newNoteWithId noteId =
            ClientOnlyNote noteId ""
    in
    newClientId seed
        |> Tuple.mapFirst newNoteWithId


updateNoteText : String -> Note -> Note
updateNoteText str note =
    case note of
        ClientOnly (ClientOnlyNote id _) ->
            ClientOnly (ClientOnlyNote id str)

        Stored (StoredNote serverId clientId _) ->
            Stored (StoredNote serverId clientId str)


encodeClientNote : NotebookId -> ClientOnlyNote -> Value
encodeClientNote notebookId (ClientOnlyNote _ content) =
    Encode.object
        [ ( "content", Encode.string content )
        , ( "notebook_id", Encode.string (notebookIdToString notebookId) )
        ]


encodeStoredNote : NotebookId -> StoredNote -> Value
encodeStoredNote notebookId (StoredNote serverId clientId content) =
    Encode.object
        [ ( "id", Encode.int serverId )
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


noteDecoder : Decoder ( ServerId, String )
noteDecoder =
    Decode.map2
        Tuple.pair
        (Decode.field "id" Decode.int)
        (Decode.field "content" Decode.string)


notesDecoder : Decoder (List ( ServerId, String ))
notesDecoder =
    Decode.list noteDecoder


storedNotesDecoder : Seed -> Decoder ( List StoredNote, Seed )
storedNotesDecoder seed =
    notesDecoder
        |> Decode.map (initializeIds seed)


firstNoteDecoder : Decoder ( ServerId, String )
firstNoteDecoder =
    singletonDecoder noteDecoder


notesEndpoint : String
notesEndpoint =
    "notes"


noteEndpoint : NotebookId -> StoredNote -> String
noteEndpoint notebookId (StoredNote serverId _ content) =
    notesEndpoint ++ "?id=eq." ++ String.fromInt serverId ++ "&notebook_id=eq." ++ notebookIdToString notebookId


insertNotes : Seed -> (Result Http.Error ( List StoredNote, Seed ) -> msg) -> NotebookId -> List ClientOnlyNote -> Cmd msg
insertNotes seed toMsg notebookId notes =
    postSupabase
        { path = notesEndpoint
        , body = encodeClientNoteList notebookId notes
        , decoder = storedNotesDecoder seed
        , toMsg = toMsg
        }


insertNewNote : ClientOnlyNote -> (Result Http.Error StoredNote -> msg) -> NotebookId -> Cmd msg
insertNewNote oldNote toMsg notebookId =
    postSupabase
        { path = notesEndpoint
        , body = encodeClientNote notebookId oldNote
        , decoder = Decode.map (storedNoteFromClientNote oldNote) firstNoteDecoder
        , toMsg = toMsg
        }


patchNote : Seed -> (Result Http.Error ( StoredNote, Seed ) -> msg) -> NotebookId -> StoredNote -> Cmd msg
patchNote seed toMsg notebookId note =
    patchSupabase
        { path = noteEndpoint notebookId note
        , body = encodeStoredNote notebookId note
        , decoder = Decode.map (newStoredNote seed) firstNoteDecoder
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

                Stored (StoredNote _ _ c) ->
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


newStoredNote : Seed -> ( ServerId, String ) -> ( StoredNote, Seed )
newStoredNote seed ( serverId, content ) =
    newClientId seed
        |> Tuple.mapFirst
            (\clientId -> StoredNote serverId clientId content)


storedNoteFromClientNote : ClientOnlyNote -> ( ServerId, String ) -> StoredNote
storedNoteFromClientNote (ClientOnlyNote clientId _) ( serverId, content ) =
    StoredNote serverId clientId content


initializeIds : Seed -> List ( ServerId, String ) -> ( List StoredNote, Seed )
initializeIds seed list =
    case list of
        [] ->
            ( [], seed )

        x :: rest ->
            let
                ( note, newSeed ) =
                    newStoredNote seed x

                ( restOfNotes, finalSeed ) =
                    initializeIds newSeed rest
            in
            ( note :: restOfNotes, finalSeed )
