module Note exposing
    ( Note
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
    = ClientOnly ClientId String
    | Stored ServerId ClientId String


type alias ClientId =
    String


type alias ServerId =
    Int


noteIdString : Note -> String
noteIdString note =
    case note of
        ClientOnly clientId _ ->
            clientId

        Stored _ clientId _ ->
            clientId


newClientId : Seed -> ( ClientId, Seed )
newClientId seed =
    Random.step wordGenerator seed
        |> Tuple.mapFirst (\noteId -> "clientId-" ++ noteId)


newNote : Seed -> ( Note, Seed )
newNote seed =
    let
        newNoteWithId : ClientId -> Note
        newNoteWithId noteId =
            ClientOnly noteId ""
    in
    newClientId seed
        |> Tuple.mapFirst newNoteWithId


updateNoteText : String -> Note -> Note
updateNoteText str note =
    case note of
        ClientOnly id _ ->
            ClientOnly id str

        Stored serverId clientId _ ->
            Stored serverId clientId str


encodeNote : NotebookId -> Note -> Value
encodeNote notebookId note =
    case note of
        ClientOnly _ content ->
            Encode.object
                [ ( "content", Encode.string content )
                , ( "notebook_id", Encode.string (notebookIdToString notebookId) )
                ]

        Stored serverId _ content ->
            Encode.object
                [ ( "id", Encode.int serverId )
                , ( "content", Encode.string content )
                , ( "notebook_id", Encode.string (notebookIdToString notebookId) )
                ]


encodeNoteList : NotebookId -> List Note -> Value
encodeNoteList notebookId notes =
    Encode.list (encodeNote notebookId) notes


noteDecoder : Decoder ( ServerId, String )
noteDecoder =
    Decode.map2
        Tuple.pair
        (Decode.field "id" Decode.int)
        (Decode.field "content" Decode.string)


notesDecoder : Decoder (List ( ServerId, String ))
notesDecoder =
    Decode.list noteDecoder


storedNotesDecoder : Seed -> Decoder ( List Note, Seed )
storedNotesDecoder seed =
    notesDecoder
        |> Decode.map (initializeIds seed)


firstNoteDecoder : Decoder ( ServerId, String )
firstNoteDecoder =
    singletonDecoder noteDecoder


notesEndpoint : String
notesEndpoint =
    "notes"


noteEndpoint : NotebookId -> ServerId -> String
noteEndpoint notebookId serverId =
    notesEndpoint ++ "?id=eq." ++ String.fromInt serverId ++ "&notebook_id=eq." ++ notebookIdToString notebookId


insertNotes : Seed -> (Result Http.Error ( List Note, Seed ) -> msg) -> NotebookId -> List Note -> Cmd msg
insertNotes seed toMsg notebookId notes =
    postSupabase
        { path = notesEndpoint
        , body = encodeNoteList notebookId notes
        , decoder = storedNotesDecoder seed
        , toMsg = toMsg
        }


insertNewNote : Note -> (Result Http.Error Note -> msg) -> NotebookId -> Cmd msg
insertNewNote oldNote toMsg notebookId =
    case oldNote of
        Stored _ _ _ ->
            Cmd.none

        ClientOnly clientId _ ->
            postSupabase
                { path = notesEndpoint
                , body = encodeNote notebookId oldNote
                , decoder = Decode.map (storedNoteFromClientNote clientId) firstNoteDecoder
                , toMsg = toMsg
                }


patchNote : Seed -> (Result Http.Error ( Note, Seed ) -> msg) -> NotebookId -> Note -> Cmd msg
patchNote seed toMsg notebookId note =
    case note of
        ClientOnly _ _ ->
            Cmd.none

        Stored serverId _ _ ->
            patchSupabase
                { path = noteEndpoint notebookId serverId
                , body = encodeNote notebookId note
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
                ClientOnly _ c ->
                    c

                Stored _ _ c ->
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
    [ ClientOnly "clientId-1" "Store model in localStorage as well so it can be started offline."
    , ClientOnly "clientId-2" "Merge a notebook's notes in a non-destructive way whenever a content conflict is detected after downloading a note."
    , ClientOnly "clientId-3" "Make notes automatically synchronized by using supabase client's real-time API on a JS port."
    , ClientOnly "clientId-4" "Make note deletion undoable."
    , ClientOnly "clientId-5" "Debounce database updates."
    , ClientOnly "clientId-6" "Regenerate notebook ID upon primary key constraint violation on insert."
    ]


newStoredNote : Seed -> ( ServerId, String ) -> ( Note, Seed )
newStoredNote seed ( serverId, content ) =
    newClientId seed
        |> Tuple.mapFirst
            (\clientId -> Stored serverId clientId content)


storedNoteFromClientNote : ClientId -> ( ServerId, String ) -> Note
storedNoteFromClientNote clientId ( serverId, content ) =
    Stored serverId clientId content


initializeIds : Seed -> List ( ServerId, String ) -> ( List Note, Seed )
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
