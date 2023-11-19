port module Main exposing (Flags, Model, Msg, main)

import Browser
import ButtonView exposing (buttonView, linkButtonView)
import ClipboardButton exposing (ClipboardMsg, ClipboardState)
import Css
import Debug exposing (todo)
import Dict exposing (Dict)
import Html.Styled exposing (Html, b, div, h1, p, span, text)
import Html.Styled.Attributes as Attributes exposing (attribute, class)
import Html.Styled.Keyed as Keyed
import Http
import Icons
import Identifiers exposing (NotebookId)
import Note exposing (Note, noteIdString, noteToPair, noteView)
import Notebook exposing (insertNotebook)
import Random
import Spinner exposing (spinner)
import Time


port updateLocation : String -> Cmd msg


{-| The application can be in two states regarding its lifecycle:

  - `OpeningNotebook`: Initial state. The app doesn't know yet if it's going to
    open an existing notebook or create a new one. The user can't edit notes, and
    instead sees a spinner where the notebook should be.
  - `OpenNotebook`: The application is already interactive and the user can edit
    notes.

-}
type Model
    = OpeningNotebook Random.Seed
    | OpenNotebook LoadedModel


type alias LoadedModel =
    { randomSeed : Random.Seed
    , connectionStatus : ConnectionStatus
    , notebookId : NotebookId
    , notes : Notes
    , clipboardState : ClipboardState
    }


{-| The notes contained in the current open Notebook.
-}
type alias Notes =
    Dict String Note


{-| The application can be in different states regarding its connection status:

  - `NotebookNotStored`: A new notebook has been created, but the app hasn't had
    connection ever since, so the notebook only exists locally.
  - `NotebookOnline`: The app is in sync with the server.
  - `NotebookOffline`: The connection has been lost, or just recovered. Some
    reconciliation may need to be done upon recovery. It keeps a record of all the
    Stored Notes that have been deleted only in the client.

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



----- INIT -----


{-| Upon starting, the application receives from JS the following data:

  - `path`: The local part of the URL. Should contain the notebook the user is
    trying to open.
  - `randomSeed`: A seed for random generation, preferably a cryptographically
    strong one (crypto.getRandomValues()), to minimize collisions.

-}
type alias Flags =
    { path : String, randomSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init { path, randomSeed } =
    let
        openNewNotebook : Cmd Msg
        openNewNotebook =
            Identifiers.fetchTwoWords (Initializing << WordsFetched)

        openExistingNotebook : NotebookId -> Cmd Msg
        openExistingNotebook notebookId =
            Notebook.checkNotebookExists (Initializing << NotebookChecked) notebookId

        openANotebook : Cmd Msg
        openANotebook =
            case String.toList path of
                [ '/' ] ->
                    openNewNotebook

                '/' :: notebookId ->
                    String.fromList notebookId
                        |> Identifiers.parseNotebookId
                        |> Result.map openExistingNotebook
                        |> Result.withDefault openNewNotebook

                _ ->
                    openNewNotebook
    in
    ( OpeningNotebook (Random.initialSeed randomSeed)
    , openANotebook
    )



----- UPDATE -----

-- TODO: Simplify Results in decoders (ServerError | ConnectionError instead of Http.Error)

{-| Messages only sent during application initialization.
-}
type InitializationMsg
    = NotebookChecked (Result Http.Error NotebookId)
    | WordsFetched (Result Http.Error ( String, String ))
    | NotebookFetched NotebookId (Result Http.Error ( List Note, Random.Seed ))


{-| Messages only sent after the application has been initialized and it is interactive.
-}
type AppMsg
    = NotebookStored (Result Http.Error NotebookId)
    | ClipboardMsgContainer ClipboardMsg
    | WriteNote Note String
    | AddNote
    | NoteStored (Result Http.Error Note)
    | NoteUpdated Note (Result Http.Error Note)
    | DeleteNote Note
    | NoteDeleted Note (Result Time.Posix ())


type Msg
    = Initializing InitializationMsg
    | Initialized AppMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initializing initMsg ->
            case model of
                OpeningNotebook randomSeed ->
                    updateOpeningNotebook randomSeed initMsg model

                OpenNotebook _ ->
                    ( model, Cmd.none )

        Initialized appMsg ->
            case model of
                OpeningNotebook _ ->
                    ( model, Cmd.none )

                OpenNotebook loadedModel ->
                    let
                        updateFunction =
                            case loadedModel.connectionStatus of
                                NotebookNotStored ->
                                    updateNotebookNotStored

                                NotebookOnline ->
                                    updateNotebookOnline

                                NotebookOffline deletedNotes ->
                                    updateNotebookOffline deletedNotes
                    in
                    updateFunction appMsg loadedModel
                        |> Tuple.mapFirst OpenNotebook
                        |> Tuple.mapSecond (Cmd.map Initialized)


updateOpeningNotebook : Random.Seed -> InitializationMsg -> Model -> ( Model, Cmd Msg )
updateOpeningNotebook randomSeed msg model =
    case msg of
        NotebookChecked result ->
            let
                networkOkButNotebookNotFound : ( Model, Cmd Msg )
                networkOkButNotebookNotFound =
                    ( model, Identifiers.fetchTwoWords (Initializing << WordsFetched) )

                networkFailed : ( Model, Cmd Msg )
                networkFailed =
                    fullyRandomNotebookId randomSeed
                        |> openNewNotebookOffline
            in
            case result of
                Ok notebookId ->
                    ( model
                    , Notebook.getNotebookNotes randomSeed (Initializing << NotebookFetched notebookId) notebookId
                    )

                Err (Http.BadStatus status) ->
                    if status < 500 then
                        networkOkButNotebookNotFound

                    else
                        networkFailed

                Err (Http.BadBody _) ->
                    networkOkButNotebookNotFound

                Err _ ->
                    networkFailed

        WordsFetched result ->
            case result of
                Ok ( a, b ) ->
                    randomNotebookIdWithWords ( a, b ) randomSeed
                        |> openNewNotebookOnline

                Err _ ->
                    fullyRandomNotebookId randomSeed
                        |> openNewNotebookOnline

        NotebookFetched notebookId result ->
            case result of
                Ok ( notes, newSeed ) ->
                    let
                        dictFromNotes : List Note -> Notes
                        dictFromNotes noteList =
                            noteList
                                |> List.map noteToPair
                                |> Dict.fromList
                    in
                    ( OpenNotebook
                        { randomSeed = newSeed
                        , connectionStatus = NotebookOnline
                        , notebookId = notebookId
                        , notes = dictFromNotes notes
                        , clipboardState = ClipboardButton.initClipboardState
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( OpenNotebook
                        { randomSeed = randomSeed
                        , connectionStatus = NotebookOffline []
                        , notebookId = notebookId
                        , notes = Dict.empty
                        , clipboardState = ClipboardButton.initClipboardState
                        }
                    , Cmd.none
                    )


updateNotebookNotStored : AppMsg -> LoadedModel -> ( LoadedModel, Cmd AppMsg )
updateNotebookNotStored msg model =
    case msg of
        NotebookStored result ->
            case result of
                Ok _ ->
                    ( { model | connectionStatus = NotebookOffline [] }, syncNotes )

                Err _ ->
                    ( { model | connectionStatus = NotebookNotStored }, Cmd.none )

        ClipboardMsgContainer clipboardMsg ->
            delegateToClipboardButton clipboardMsg model

        _ ->
            Debug.todo "Complete updateNotebookNotStored"


updateNotebookOnline : AppMsg -> LoadedModel -> ( LoadedModel, Cmd AppMsg )
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
                ( newNote, newSeed ) =
                    Note.newNote model.randomSeed

                noteId : String
                noteId =
                    Note.noteIdString newNote
            in
            ( { model | randomSeed = newSeed, notes = Dict.insert noteId newNote notes }
            , Note.insertNewNote newNote NoteStored notebookId
            )

        NoteStored result ->
            case result of
                Ok storedNote ->
                    ( { model | notes = updateNoteToStored storedNote notes }
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


updateNotebookOffline : DeletedNotes -> AppMsg -> LoadedModel -> ( LoadedModel, Cmd AppMsg )
updateNotebookOffline deletedNotes msg { notebookId, notes } =
    Debug.todo "Complete updateNotebookOffline"


delegateToClipboardButton : ClipboardMsg -> LoadedModel -> ( LoadedModel, Cmd AppMsg )
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


{-| Updates the notebook to reflect the notebook is open.
Updates the location and tries to store the notebook.
-}
openNewNotebookOnline : ( Random.Seed, NotebookId ) -> ( Model, Cmd Msg )
openNewNotebookOnline ( newSeed, notebookId ) =
    ( OpenNotebook
        { randomSeed = newSeed
        , connectionStatus = NotebookNotStored
        , notebookId = notebookId
        , notes = Dict.empty
        , clipboardState = ClipboardButton.initClipboardState
        }
    , Cmd.batch
        [ updateLocation (Identifiers.notebookIdToString notebookId)
        , insertNotebook (Initialized << NotebookStored) notebookId
        ]
    )


{-| Updates the model to reflect the notebook is open.
Does not update the URL, does not try to store the notebook.
-}
openNewNotebookOffline : ( Random.Seed, NotebookId ) -> ( Model, Cmd Msg )
openNewNotebookOffline ( newSeed, notebookId ) =
    ( OpenNotebook
        { randomSeed = newSeed
        , connectionStatus = NotebookNotStored
        , notebookId = notebookId
        , notes = Dict.empty
        , clipboardState = ClipboardButton.initClipboardState
        }
    , Cmd.none
    )


{-| Returns a NotebookId made with the supplied two words and a randomly
generated one.
-}
randomNotebookIdWithWords : ( String, String ) -> Random.Seed -> ( Random.Seed, NotebookId )
randomNotebookIdWithWords ( a, b ) seed =
    let
        ( c, newSeed ) =
            Random.step Identifiers.wordGenerator seed
    in
    ( newSeed, Identifiers.notebookIdFromWords a b c )


{-| Returns a NotebookId made from three randomly generated words.
-}
fullyRandomNotebookId : Random.Seed -> ( Random.Seed, NotebookId )
fullyRandomNotebookId seed =
    let
        ( a, seed1 ) =
            Random.step Identifiers.wordGenerator seed

        ( b, seed2 ) =
            Random.step Identifiers.wordGenerator seed1
    in
    randomNotebookIdWithWords ( a, b ) seed2


syncNotes : Cmd AppMsg
syncNotes =
    todo "Download up-to-date notes, replace updated notes, duplicate conflicts, delete notes deleted locally, and store new notes and conflicts"


updateNoteToStored : Note -> Dict String Note -> Dict String Note
updateNoteToStored note notes =
    let
        updateNote : Maybe Note -> Maybe Note
        updateNote =
            Maybe.map (\_ -> note)
    in
    Dict.update (noteIdString note) updateNote notes



----- VIEW -----


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


{-| Displays the NotebookId along with a copy-to-clipboard button.
-}
notebookIdView : NotebookId -> ClipboardState -> Html AppMsg
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


notebookView : LoadedModel -> Html AppMsg
notebookView { notebookId, notes, clipboardState } =
    let
        notesList : List ( String, Html AppMsg )
        notesList =
            Dict.toList notes
                |> List.map Tuple.second
                |> List.sortWith Note.compareNoteOrder
                |> List.map
                    (\note ->
                        noteView
                            { note = note
                            , onInput = WriteNote
                            , onDelete = DeleteNote
                            }
                    )

        addNoteButton : Html AppMsg
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


view : Model -> Html AppMsg
view model =
    let
        notebook : Html AppMsg
        notebook =
            case model of
                OpeningNotebook _ ->
                    spinner

                OpenNotebook loadedModel ->
                    notebookView loadedModel
    in
    div [ class "screen" ]
        [ div [ class "notebookScreen" ]
            [ h1 [ class "title" ] [ text "Elm Shared Notes" ]
            , notebook
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view =
            \model ->
                view model
                    |> Html.Styled.map Initialized
                    |> Html.Styled.toUnstyled
        , subscriptions = \_ -> Sub.none
        }
