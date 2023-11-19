port module Main exposing (Flags, Model, Msg, App, main)

import Browser
import ButtonView exposing (linkButtonView, buttonView)
import Css
import Dict exposing (Dict)
import Html.Styled exposing (Html, b, div, h1, p, span, text)
import Html.Styled.Attributes as Attributes exposing (class, attribute)
import Html.Styled.Keyed as Keyed
import Http
import Icons
import Identifiers exposing (NotebookId)
import Note exposing (Note, noteIdString, noteToPair, noteView)
import Notebook exposing (insertNotebook)
import Random
import Spinner exposing (spinner)
import ClipboardButton exposing (ClipboardState, ClipboardMsg)
import Time
import Debug exposing (todo)

port updateLocation : String -> Cmd msg


type alias Model =
    { randomSeed : Random.Seed
    , app : App
    , clipboardState : ClipboardState
    }


{-| The application can be in different states regarding its connection status:

- `OpeningNotebook`: Initial state. The app doesn't know yet if it's going to 
open an existing notebook or create a new one.
- `NotebookNotStored`: A new notebook has been created, but the app hasn't had
connection ever since, so the notebook only exists locally.
- `NotebookOnline`: The app is in sync with the server.
- `NotebookOffline`: The connection has been lost, or just recovered. Some 
reconciliation may need to be done upon recovery.
-}
type App
    = OpeningNotebook
    | NotebookNotStored NotebookId Notes
    | NotebookOnline NotebookId Notes
    | NotebookOffline NotebookId Notes DeletedNotes

{-| The notes contained in the current open Notebook.
-}
type alias Notes =
    Dict String Note

{-| A list of deleted notes, pending to be deleted from the remote database
too.
Each note is kept with the timestamp of its deletion.
-}
type alias DeletedNotes = List (Time.Posix, Note)


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
        newNotebook : ( App, Cmd Msg )
        newNotebook =
            ( OpeningNotebook
            , Identifiers.fetchTwoWords (Initializing << WordsFetched)
            )

        existingNotebook : NotebookId -> ( App, Cmd Msg )
        existingNotebook notebookId =
            ( OpeningNotebook
            , Notebook.checkNotebookExists (Initializing << NotebookChecked) notebookId
            )

        newApp : ( App, Cmd Msg )
        newApp =
            case String.toList path of
                [ '/' ] ->
                    newNotebook

                '/' :: notebookId ->
                    String.fromList notebookId
                        |> Identifiers.parseNotebookId
                        |> Result.map existingNotebook
                        |> Result.withDefault newNotebook

                _ ->
                    newNotebook
    in
    ( { randomSeed = Random.initialSeed randomSeed
      , app = Tuple.first newApp
      , clipboardState = ClipboardButton.initClipboardState
      }
    , Tuple.second newApp
    )


----- UPDATE -----

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
            case model.app of
                OpeningNotebook -> updateOpeningNotebook initMsg model

                NotebookNotStored _ _ -> ( model, Cmd.none )

                NotebookOnline _ _ -> ( model, Cmd.none )

                NotebookOffline _ _ _ -> ( model, Cmd.none )

        Initialized appMsg ->
            case model.app of
                OpeningNotebook -> ( model, Cmd.none )

                NotebookNotStored notebookId notes ->
                    updateNotebookNotStored appMsg model notebookId notes
                        |> Tuple.mapSecond (Cmd.map Initialized)

                NotebookOnline notebookId notes ->
                    updateNotebookOnline appMsg model notebookId notes
                        |> Tuple.mapSecond (Cmd.map Initialized)

                NotebookOffline notebookId notes deletedNotes ->
                    updateNotebookOffline appMsg model notebookId notes deletedNotes
                        |> Tuple.mapSecond (Cmd.map Initialized)


updateOpeningNotebook : InitializationMsg -> Model -> ( Model, Cmd Msg )
updateOpeningNotebook msg model =
    case msg of
        NotebookChecked result ->
            let
                networkOkButNotebookNotFound : ( Model, Cmd Msg )
                networkOkButNotebookNotFound = ( model, Identifiers.fetchTwoWords (Initializing << WordsFetched) )

                networkFailed : ( Model, Cmd Msg )
                networkFailed = fullyRandomNotebookId model.randomSeed
                    |> openNewNotebookOffline
            in
                case result of
                    Ok notebookId ->
                        ( model
                        , Notebook.getNotebookNotes model.randomSeed (Initializing << (NotebookFetched notebookId)) notebookId
                        )

                    Err (Http.BadStatus status) ->
                        if status < 500
                            then networkOkButNotebookNotFound
                            else networkFailed

                    Err (Http.BadBody _) ->
                        networkOkButNotebookNotFound

                    Err _ ->
                        networkFailed

        WordsFetched result ->
            case result of
                Ok ( a, b ) ->
                    randomNotebookIdWithWords ( a, b ) model.randomSeed
                        |> openNewNotebookOnline

                Err _ ->
                    fullyRandomNotebookId model.randomSeed
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
                        ( { model
                            | app = NotebookOnline notebookId (dictFromNotes notes)
                            , randomSeed = newSeed
                        }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | app = NotebookOffline notebookId Dict.empty [] }
                    , Cmd.none
                    )


updateNotebookNotStored : AppMsg -> Model -> NotebookId -> Notes -> ( Model, Cmd AppMsg )
updateNotebookNotStored msg model notebookId notes =
    case msg of
        NotebookStored result ->
            case result of
                Ok _ -> ( {model | app = NotebookOffline notebookId notes []}, syncNotes )

                Err _ -> ( {model | app = NotebookNotStored notebookId notes}, Cmd.none )

        ClipboardMsgContainer clipboardMsg ->
            delegateToClipboardButton notebookId clipboardMsg model

        _ ->
            Debug.todo "Complete updateNotebookNotStored"

updateNotebookOnline : AppMsg -> Model -> NotebookId -> Notes -> ( Model, Cmd AppMsg )
updateNotebookOnline msg model notebookId notes =
    case msg of
        NotebookStored result ->
            ( model, Cmd.none )

        ClipboardMsgContainer clipboardMsg ->
            delegateToClipboardButton notebookId clipboardMsg model

        WriteNote note value ->
            let
                newNote : Note
                newNote = Note.updateNoteText value note
            in
            ( mapModelNotes (updateNoteInNotebook newNote) model
            , Note.upsertNote (NoteUpdated newNote) notebookId newNote
            )

        AddNote ->
            let
                ( newNote, newSeed ) =
                    Note.newNote model.randomSeed

                noteId : String
                noteId =
                    Note.noteIdString newNote

                newApp : App
                newApp =
                    NotebookOnline notebookId (Dict.insert noteId newNote notes)
            in
            ( { model | randomSeed = newSeed, app = newApp }
            , Note.insertNewNote newNote NoteStored notebookId
            )

        NoteStored result ->
            case result of
                Ok storedNote ->
                    ( mapModelNotes (updateNoteToStored storedNote) model
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | app = NotebookOffline notebookId notes []
                    }
                    , Cmd.none
                    )

        NoteUpdated oldNote result ->
            case result of
                Ok newNote ->
                    
                    ( mapModelNotes (updateNoteInNotebook newNote) model
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | app = NotebookOffline notebookId notes []
                    }
                    , Cmd.none
                    )

        DeleteNote note ->
            ( { model | app = NotebookOnline notebookId (Dict.remove (Note.noteIdString note) notes) }
            , Note.deleteNote (NoteDeleted note) notebookId note
            )

        NoteDeleted note result ->
            case result of
                Ok () ->
                    ( model, Cmd.none )

                Err timestamp ->
                    ( { model
                        | app = NotebookOffline notebookId notes [(timestamp, note)]
                    }
                    , Cmd.none
                    )

mapModelNotes : (Notes -> Notes) -> Model -> Model
mapModelNotes fn model =
    let
        newApp = case model.app of
            OpeningNotebook -> OpeningNotebook

            NotebookNotStored notebookId notes -> NotebookNotStored notebookId (fn notes)

            NotebookOnline notebookId notes -> NotebookOnline notebookId (fn notes)

            NotebookOffline notebookId notes deletedNotes -> NotebookOffline notebookId (fn notes) deletedNotes
    in
        { model | app = newApp }

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

updateNotebookOffline : AppMsg -> Model -> NotebookId -> Notes -> DeletedNotes -> ( Model, Cmd AppMsg )
updateNotebookOffline msg model notebookId notes deletedNotes =
    Debug.todo "Complete updateNotebookOffline"

delegateToClipboardButton : NotebookId -> ClipboardMsg -> Model -> ( Model, Cmd AppMsg )
delegateToClipboardButton notebookId clipboardMsg model =
    let
        (newClipboardState, clipboardCmd) = ClipboardButton.updateClipboardState
            notebookId
            clipboardMsg
            model.clipboardState
    in
        ( { model | clipboardState = newClipboardState }
        , Cmd.map ClipboardMsgContainer clipboardCmd
        )

{-| Updates the notebook to reflect the notebook is open.
Updates the location and tries to store the notebook.
-}
openNewNotebookOnline : (Random.Seed, NotebookId) -> ( Model, Cmd Msg )
openNewNotebookOnline (newSeed, notebookId) =
    ( { randomSeed = newSeed
      , app = NotebookNotStored notebookId Dict.empty
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
openNewNotebookOffline : (Random.Seed, NotebookId) -> (Model, Cmd Msg)
openNewNotebookOffline (newSeed, notebookId) =
    ( { randomSeed = newSeed
      , app = NotebookNotStored notebookId Dict.empty
      , clipboardState = ClipboardButton.initClipboardState
      }
    , Cmd.none
    )

{-| Returns a NotebookId made with the supplied two words and a randomly
generated one.
-}
randomNotebookIdWithWords : ( String, String ) -> Random.Seed -> (Random.Seed, NotebookId)
randomNotebookIdWithWords (a, b) seed =
    let
        ( c, newSeed ) =
            Random.step Identifiers.wordGenerator seed
    in
        (newSeed, Identifiers.notebookIdFromWords a b c)

{-| Returns a NotebookId made from three randomly generated words.
-}
fullyRandomNotebookId : Random.Seed -> (Random.Seed, NotebookId)
fullyRandomNotebookId seed =
    let
        ( a, seed1 ) =
            Random.step Identifiers.wordGenerator seed

        ( b, seed2 ) =
            Random.step Identifiers.wordGenerator seed1
    in
        randomNotebookIdWithWords (a, b) seed2

syncNotes : Cmd AppMsg
syncNotes = todo "Download up-to-date notes, replace updated notes, duplicate conflicts, delete notes deleted locally, and store new notes and conflicts"

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
        idString = Identifiers.notebookIdToString notebookId
    in
        div [ class "notebookId", attribute "data-testid" "notebookId" ]
            [ span [] [ text idString ]
            , ClipboardButton.clipboardButtonView state
                |> Html.Styled.map ClipboardMsgContainer
            ]

notebookView : NotebookId -> Notes -> ClipboardState -> Html AppMsg
notebookView notebookId notes clipboardState =
    let
        notesList : List ( String, Html AppMsg )
        notesList =
            Dict.toList notes
                |> List.map Tuple.second
                |> List.sortWith Note.compareNoteOrder
                |> List.map (\note -> noteView
                    { note = note
                    , onInput = WriteNote
                    , onDelete = DeleteNote
                    })

        addNoteButton : Html AppMsg
        addNoteButton = buttonView
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
            case model.app of
                OpeningNotebook ->
                    spinner

                NotebookNotStored notebookId notes ->
                    notebookView notebookId notes model.clipboardState

                NotebookOnline notebookId notes ->
                    notebookView notebookId notes model.clipboardState

                NotebookOffline notebookId notes _ ->
                    notebookView notebookId notes model.clipboardState
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
        , view = \model -> view model
            |> Html.Styled.map Initialized
            |> Html.Styled.toUnstyled
        , subscriptions = \_ -> Sub.none
        }
