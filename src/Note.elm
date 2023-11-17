module Note exposing
    ( Note
    , insertNewNote
    , insertNotes
    , newNote
    , noteIdString
    , noteToPair
    , noteView
    , patchNote
    , deleteNote
    , storedNotesDecoder
    , updateNoteText
    , compareNoteOrder
    )

import AutoTextarea
import Html.Styled exposing (Html)
import Http
import Identifiers exposing (NotebookId, notebookIdToString, wordGenerator)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Random exposing (Seed)
import Supabase exposing (patchSupabase, postSupabase, deleteSupabase, singletonDecoder)

{-| An editable note. Contains some text and internal metadata.

Notes can be in two states:

- A `ClientOnly` note has never been synced with the server. It has a
ClientId for referencing, guaranteed to be unique in this device.
- A `Stored` note has at least once been synced. It has a ServerId guaranteed
to be unique among all notes in this notebook. It keeps its ClientId, because
it is their real ID as far as the client is concerned.

A note can be promoted to Stored by adding the ID the server assigned to it,
but it can never go back to ClientOnly. Otherwise, it would get duplicated when
syncing.
-}
type Note
    = ClientOnly ClientId String
    | Stored ServerId ClientId String


{-| The ClientId is a Note ID that uniquely identifies a note in this device. It is never stored remotely.

A ClientId is generated when:

- The user adds a new note.
- The application syncs with the server and new notes are received, in which
case they are all given new ClientIds.
- The application loads a notebook from the server, in which case all its notes
are given new ClientIds.
-}
type alias ClientId =
    String

{-| The ServerId is a Note ID that uniquely identifies a note in the remote database.

It can not be used as a reference because some notes may not be stored yet, and it is not guaranteed to be unique among ClientIds.

The ServerId should only be used to build requests that update the note, never to reference notes in the application.
-}
type alias ServerId =
    Int


noteIdString : Note -> String
noteIdString note =
    case note of
        ClientOnly clientId _ ->
            clientId

        Stored _ clientId _ ->
            clientId


-- TODO: Make ClientId unique via an auto-increment.
newClientId : Seed -> ( ClientId, Seed )
newClientId seed =
    Random.step wordGenerator seed
        |> Tuple.mapFirst (\noteId -> "clientId-" ++ noteId)

{-| A new Note with a generated ClientId.
-}
newNote : Seed -> ( Note, Seed )
newNote seed =
    let
        newNoteWithId : ClientId -> Note
        newNoteWithId noteId =
            ClientOnly noteId ""
    in
    newClientId seed
        |> Tuple.mapFirst newNoteWithId

{-| The same Note with this new text.
-}
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

{-| Decodes only the first note in a list. Used in the cases we know Supabase
is going to return a list with only one note.
-}
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

deleteNote : (Result Http.Error () -> msg) -> NotebookId -> Note -> Cmd msg
deleteNote toMsg notebookId note =
    case note of
        ClientOnly _ _ ->
            Cmd.none

        Stored serverId _ _ ->
            deleteSupabase
                { path = noteEndpoint notebookId serverId
                , toMsg = toMsg
                }

{-| Provides the Note as an (ID, Note) tuple for indexing purposes (for
example, a Dictionary).
-}
noteToPair : Note -> ( String, Note )
noteToPair note =
    ( noteIdString note, note )

{-| Preserve the order by relying on the serverIds auto-increment.
TODO: Provide notes of createdAt field.
-}
compareNoteOrder : Note -> Note -> Order
compareNoteOrder a b =
    case (a, b) of
        (Stored serverIdA _ _, Stored serverIdB _ _) -> compare serverIdA serverIdB
        (Stored _ _ _, ClientOnly _ _) -> LT
        (ClientOnly _ _, Stored _ _ _) -> GT
        (ClientOnly _ _, ClientOnly _ _) -> EQ

{-| Displays the Note as an editable auto-rezised textarea.
-}
noteView :
    { note : Note
    , onInput : Note -> String -> msg
    , onDelete : Note -> msg
    }
    -> ( String, Html msg )
noteView { note, onInput, onDelete } =
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
        , onInput = onInput note
        , onDelete = onDelete note
        , placeholder = ""
        }
    )

{-
exampleNotes : List Note
exampleNotes =
    [ ClientOnly "clientId-1" "Store model in localStorage as well so it can be started offline."
    , ClientOnly "clientId-2" "Merge a notebook's notes in a non-destructive way whenever a content conflict is detected after downloading a note."
    , ClientOnly "clientId-3" "Make notes automatically synchronized by using supabase client's real-time API on a JS port."
    , ClientOnly "clientId-4" "Make note deletion undoable."
    , ClientOnly "clientId-5" "Debounce database updates."
    , ClientOnly "clientId-6" "Regenerate notebook ID upon primary key constraint violation on insert."
    ]
-}

{-| Creates a Note from its downloaded data by generating a new ClientId.
-}
newStoredNote : Seed -> ( ServerId, String ) -> ( Note, Seed )
newStoredNote seed ( serverId, content ) =
    newClientId seed
        |> Tuple.mapFirst
            (\clientId -> Stored serverId clientId content)

{-| Creates a Note from its downloaded data by giving it an existing ClientId.
-}
storedNoteFromClientNote : ClientId -> ( ServerId, String ) -> Note
storedNoteFromClientNote clientId ( serverId, content ) =
    Stored serverId clientId content


{-| Creates several Notes from their downloaded data by generating new
ClientIds.
-}
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
