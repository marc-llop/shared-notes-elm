module OpenNotebook exposing (ConnectionStatus(..), Model, Msg(..), emptyNotes, noteListToNotes, update, view)

import ButtonView exposing (buttonView, linkButtonView)
import ClipboardButton exposing (ClipboardMsg, ClipboardState)
import Css
import Debug exposing (todo)
import Dict exposing (Dict)
import Html.Styled exposing (Html, b, div, h1, p, span, text)
import Html.Styled.Attributes as Attributes exposing (attribute, class)
import Html.Styled.Keyed as Keyed
import Icons
import Identifiers exposing (NotebookId)
import Note exposing (Note)
import Supabase exposing (CallError(..))
import Time


type alias Model =
    { smallestAvailableId : Int
    , connectionStatus : ConnectionStatus
    , notebookId : NotebookId
    , notes : Notes
    , clipboardState : ClipboardState
    }


{-| The notes contained in the current open Notebook.
-}
type alias Notes =
    Dict String Note


noteListToNotes : List Note -> Notes
noteListToNotes noteList =
    noteList
        |> List.map Note.noteToPair
        |> Dict.fromList


emptyNotes : Notes
emptyNotes =
    Dict.empty


{-| The application can be in different states regarding its connection status:

  - `NotebookNotStored`: A new notebook has been created, but the app hasn't
    had connection ever since, so the notebook only exists locally.
  - `NotebookOnline`: The app is in sync with the server.
  - `NotebookOffline`: The connection has been lost, or just recovered. Some
    reconciliation may need to be done upon recovery. It keeps a record of all
    the Stored Notes that have been deleted only in the client.

While being in `NotebookOffline`, the application will still attempt to store
each change by calling the remote database, and whenever a request succeeds,
the application will resolve all conflicts and go back to `NotebookOnline`.

-}
type ConnectionStatus
    = NotebookNotStored
    | NotebookOnline
    | NotebookOffline DeletedNotes


{-| A list of deleted notes, pending to be deleted from the remote database
too.
Each note is kept with the timestamp of its deletion.
-}
type alias DeletedNotes =
    List ( Time.Posix, Note )



----- UPDATE -----


{-| Messages only sent after the application has been initialized and it is interactive.
-}
type Msg
    = NotebookStored (Result CallError NotebookId)
    | ClipboardMsgContainer ClipboardMsg
    | WriteNote Note String
    | AddNote
    | NoteStored (Result CallError Note)
    | NoteUpdated Note (Result CallError Note)
    | DeleteNote Note
    | NoteDeleted Note (Result Time.Posix ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.connectionStatus of
        NotebookNotStored ->
            updateNotebookNotStored msg model

        NotebookOnline ->
            updateNotebookOnline msg model

        NotebookOffline deletedNotes ->
            updateNotebookOffline deletedNotes msg model


updateNotebookNotStored : Msg -> Model -> ( Model, Cmd Msg )
updateNotebookNotStored msg model =
    case msg of
        NotebookStored result ->
            case result of
                Ok _ ->
                    ( { model | connectionStatus = NotebookOffline [] }, syncNotes () )

                Err _ ->
                    ( { model | connectionStatus = NotebookNotStored }, Cmd.none )

        ClipboardMsgContainer clipboardMsg ->
            delegateToClipboardButton clipboardMsg model

        _ ->
            Debug.todo "Complete updateNotebookNotStored"


updateNotebookOnline : Msg -> Model -> ( Model, Cmd Msg )
updateNotebookOnline msg ({ notebookId, notes } as model) =
    case msg of
        NotebookStored _ ->
            ( model, Cmd.none )

        ClipboardMsgContainer clipboardMsg ->
            delegateToClipboardButton clipboardMsg model

        WriteNote note value ->
            let
                newNote : Note
                newNote =
                    Note.updateNoteText value note
            in
            ( { model | notes = updateNoteInNotebook newNote notes }
            , Note.upsertNote (NoteUpdated newNote) notebookId newNote
            )

        AddNote ->
            let
                ( newNote, newSmallestAvailableId ) =
                    Note.newNote model.smallestAvailableId

                noteId : String
                noteId =
                    Note.noteIdString newNote
            in
            ( { model | smallestAvailableId = newSmallestAvailableId, notes = Dict.insert noteId newNote notes }
            , Note.insertNewNote newNote NoteStored notebookId
            )

        NoteStored result ->
            case result of
                Ok storedNote ->
                    ( { model | notes = updateNoteInNotebook storedNote notes }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | connectionStatus = NotebookOffline [] }
                    , Cmd.none
                    )

        NoteUpdated oldNote result ->
            case result of
                Ok newNote ->
                    ( { model | notes = updateNoteInNotebook newNote notes }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | connectionStatus = NotebookOffline [] }
                    , Cmd.none
                    )

        DeleteNote note ->
            ( { model | notes = Dict.remove (Note.noteIdString note) notes }
            , Note.deleteNote (NoteDeleted note) notebookId note
            )

        NoteDeleted note result ->
            case result of
                Ok () ->
                    ( model, Cmd.none )

                Err timestamp ->
                    ( { model | connectionStatus = NotebookOffline [ ( timestamp, note ) ] }
                    , Cmd.none
                    )


updateNoteInNotebook : Note -> Notes -> Notes
updateNoteInNotebook newNote notes =
    let
        noteId : String
        noteId =
            Note.noteIdString newNote

        updateInDict : Maybe Note -> Maybe Note
        updateInDict =
            Maybe.map (\_ -> newNote)
    in
    Dict.update noteId updateInDict notes


updateNotebookOffline : DeletedNotes -> Msg -> Model -> ( Model, Cmd Msg )
updateNotebookOffline deletedNotes msg { notebookId, notes } =
    Debug.todo "Complete updateNotebookOffline"


delegateToClipboardButton : ClipboardMsg -> Model -> ( Model, Cmd Msg )
delegateToClipboardButton clipboardMsg ({ notebookId, clipboardState } as model) =
    let
        ( newClipboardState, clipboardCmd ) =
            ClipboardButton.updateClipboardState
                notebookId
                clipboardMsg
                clipboardState
    in
    ( { model | clipboardState = newClipboardState }
    , Cmd.map ClipboardMsgContainer clipboardCmd
    )


syncNotes : () -> Cmd Msg
syncNotes () =
    todo "Download up-to-date notes, replace updated notes, duplicate conflicts, delete notes deleted locally, and store new notes and conflicts"



----- VIEW -----


view : Model -> Html Msg
view { notebookId, notes, clipboardState } =
    let
        notesList : List ( String, Html Msg )
        notesList =
            Dict.toList notes
                |> List.map Tuple.second
                |> List.sortWith Note.compareNoteOrder
                |> List.map
                    (\note ->
                        Note.noteView
                            { note = note
                            , onInput = WriteNote
                            , onDelete = DeleteNote
                            }
                    )

        addNoteButton : Html Msg
        addNoteButton =
            buttonView
                { icon = Icons.plus
                , onClick = AddNote
                , description = "Add Note"
                }
    in
    div [ class "notebook" ]
        [ notebookIdView notebookId clipboardState
        , p [] [ b [] [ text "Warning: " ], text "All notebooks are PUBLIC." ]
        , p [] [ text "Be mindful of what you write here. Never write any personal information, passwords, or any information you want to protect." ]
        , buttonRow
        , Keyed.ul [ class "notesList" ] notesList
        , addNoteButton
        ]


{-| Displays the NotebookId along with a copy-to-clipboard button.
-}
notebookIdView : NotebookId -> ClipboardState -> Html Msg
notebookIdView notebookId state =
    let
        idString : String
        idString =
            Identifiers.notebookIdToString notebookId
    in
    div [ class "notebookId", attribute "data-testid" "notebookId" ]
        [ span [] [ text idString ]
        , ClipboardButton.clipboardButtonView state
            |> Html.Styled.map ClipboardMsgContainer
        ]


{-| Displays a row of icon-buttons for information of interest, like the link
to this project's GitHub.
-}
buttonRow : Html msg
buttonRow =
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            ]
        ]
        [ linkButtonView
            { icon = Icons.github
            , href = "https://github.com/marc-llop/shared-notes-elm"
            , description = "GitHub"
            }
        ]
