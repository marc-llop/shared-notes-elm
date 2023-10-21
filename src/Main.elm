port module Main exposing (Flags, Model, Msg, App, main)

import Browser
import ButtonView exposing (buttonLinkView, buttonView)
import Css
import Dict exposing (Dict)
import Html.Styled exposing (Html, b, div, h1, p, span, text)
import Html.Styled.Attributes as Attributes exposing (class)
import Html.Styled.Keyed as Keyed
import Http
import Icons
import Identifiers exposing (NotebookId)
import Note exposing (Note, noteIdString, noteToPair, noteView)
import Notebook exposing (insertNotebook)
import Random
import Spinner exposing (spinner)
import ClipboardButton exposing (ClipboardState, ClipboardMsg)


port updateLocation : String -> Cmd msg


type alias Notes =
    Dict String Note

type App
    = OpeningNotebook
    | NotebookOpen NotebookId Notes

type alias Model =
    { randomSeed : Random.Seed
    , app : App
    , clipboardState : ClipboardState
    }


dictFromNotes : List Note -> Notes
dictFromNotes noteList =
    noteList
        |> List.map noteToPair
        |> Dict.fromList


type alias Flags =
    { path : String, randomSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init { path, randomSeed } =
    let
        newNotebook : ( App, Cmd Msg )
        newNotebook =
            ( OpeningNotebook
            , Identifiers.fetchTwoWords WordsFetched
            )

        existingNotebook : NotebookId -> ( App, Cmd Msg )
        existingNotebook notebookId =
            ( OpeningNotebook
            , Notebook.checkNotebookExists NotebookFound notebookId
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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = \model -> view model |> Html.Styled.toUnstyled
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = WordsFetched (Result Http.Error ( String, String ))
    | NotebookFound (Result Http.Error NotebookId)
    | NotebookFetched NotebookId (Result Http.Error ( List Note, Random.Seed ))
    | NotebookStored (Result Http.Error NotebookId)
    | ClipboardMsgContainer ClipboardMsg
    | WriteNote Note String
    | AddNote
    | NoteStored (Result Http.Error Note)
    | NoteUpdated Note (Result Http.Error ( Note, Random.Seed ))
    | DeleteNote Note
    | NoteDeleted Note (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.app of
        OpeningNotebook ->
            updateOpeningNotebook msg model

        NotebookOpen notebookId notes ->
            updateOpenNotebook msg model notebookId notes


updateOpeningNotebook : Msg -> Model -> ( Model, Cmd Msg )
updateOpeningNotebook msg model =
    case msg of
        WordsFetched result ->
            case result of
                Ok ( a, b ) ->
                    generateNotebookIdFromWords ( a, b ) model.randomSeed

                Err _ ->
                    generateNotebookIdWithoutWords model.randomSeed

        NotebookFound result ->
            case result of
                Ok notebookId ->
                    ( model
                    , Notebook.getNotebookNotes model.randomSeed (NotebookFetched notebookId) notebookId
                    )

                Err _ ->
                    ( model, Identifiers.fetchTwoWords WordsFetched )

        NotebookFetched notebookId result ->
            case result of
                Ok ( notes, newSeed ) ->
                    ( { model
                        | app = NotebookOpen notebookId (dictFromNotes notes)
                        , randomSeed = newSeed
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | app = NotebookOpen notebookId Dict.empty }
                    , Cmd.none
                    )

        NotebookStored _ ->
            ( model, Cmd.none )

        ClipboardMsgContainer _ ->
            ( model, Cmd.none )

        WriteNote _ _ ->
            ( model, Cmd.none )

        AddNote ->
            ( model, Cmd.none )

        NoteStored _ ->
            ( model, Cmd.none )

        NoteUpdated _ _ ->
            ( model, Cmd.none )

        DeleteNote _ ->
            ( model, Cmd.none )

        NoteDeleted _ _ ->
            ( model, Cmd.none )


updateOpenNotebook : Msg -> Model -> NotebookId -> Notes -> ( Model, Cmd Msg )
updateOpenNotebook msg model notebookId notes =
    case msg of
        WordsFetched _ ->
            ( model, Cmd.none )

        NotebookFound _ ->
            ( model, Cmd.none )

        NotebookFetched _ _ ->
            ( model, Cmd.none )

        NotebookStored _ ->
            ( model, Cmd.none )

        ClipboardMsgContainer clipboardMsg ->
            let
                (newClipboardState, clipboardCmd) = ClipboardButton.updateClipboardState notebookId clipboardMsg model.clipboardState
            in
                ( { model | clipboardState = newClipboardState }
                , Cmd.map ClipboardMsgContainer clipboardCmd
                )

        WriteNote note value ->
            let
                noteId : String
                noteId = Note.noteIdString note

                newNote : Note
                newNote = Note.updateNoteText value note

                updateInDict : Maybe Note -> Maybe Note
                updateInDict =
                    Maybe.map (\_ -> newNote)
            in
            ( { model
                | app =
                    NotebookOpen notebookId
                        (Dict.update noteId updateInDict notes)
              }
            , Note.patchNote model.randomSeed (NoteUpdated newNote) notebookId newNote
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
                    NotebookOpen notebookId (Dict.insert noteId newNote notes)
            in
            ( { model | randomSeed = newSeed, app = newApp }
            , Note.insertNewNote newNote NoteStored notebookId
            )

        NoteStored result ->
            case result of
                Ok storedNote ->
                    ( { model
                        | app = NotebookOpen notebookId (updateNoteToStored storedNote notes)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        NoteUpdated oldNote result ->
            case result of
                Ok ( newNote, newSeed ) ->
                    ( { model | randomSeed = newSeed }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        DeleteNote note ->
            ( model
            , Note.deleteNote (NoteDeleted note) notebookId note
            )

        NoteDeleted note result ->
            case result of
                Ok () ->
                    ( { model | app = NotebookOpen notebookId (Dict.remove (Note.noteIdString note) notes) }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

openNewNotebookInModel : Random.Seed -> NotebookId -> ( Model, Cmd Msg )
openNewNotebookInModel newSeed notebookId =
    ( { randomSeed = newSeed
      , app = NotebookOpen notebookId Dict.empty
      , clipboardState = ClipboardButton.initClipboardState
      }
    , Cmd.batch
        [ updateLocation (Identifiers.notebookIdToString notebookId)
        , insertNotebook NotebookStored notebookId
        ]
    )


generateNotebookIdFromWords : ( String, String ) -> Random.Seed -> ( Model, Cmd Msg )
generateNotebookIdFromWords ( a, b ) seed =
    let
        ( c, newSeed ) =
            Random.step Identifiers.wordGenerator seed

        notebookId : NotebookId
        notebookId =
            Identifiers.notebookIdFromWords a b c
    in
    openNewNotebookInModel newSeed notebookId


generateNotebookIdWithoutWords : Random.Seed -> ( Model, Cmd Msg )
generateNotebookIdWithoutWords seed =
    let
        ( a, seed1 ) =
            Random.step Identifiers.wordGenerator seed

        ( b, seed2 ) =
            Random.step Identifiers.wordGenerator seed1
    in
    generateNotebookIdFromWords ( a, b ) seed2


updateNoteToStored : Note -> Dict String Note -> Dict String Note
updateNoteToStored note notes =
    let
        updateNote : Maybe Note -> Maybe Note
        updateNote =
            Maybe.map (\_ -> note)
    in
    Dict.update (noteIdString note) updateNote notes


buttonRow : Html msg
buttonRow =
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            ]
        ]
        [ buttonLinkView
            { icon = Icons.github
            , href = "https://github.com/marc-llop/shared-notes-elm"
            , description = "GitHub"
            }
        ]

notebookIdView : NotebookId -> ClipboardState -> Html Msg
notebookIdView notebookId state =
    let
        idString : String
        idString = Identifiers.notebookIdToString notebookId
    in
        div [ class "notebookId" ]
            [ span [] [ text idString ]
            , ClipboardButton.clipboardButtonView state
                |> Html.Styled.map ClipboardMsgContainer
            ]

openNotebook : NotebookId -> Notes -> ClipboardState -> Html Msg
openNotebook notebookId notes clipboardState =
    let
        notesList : List ( String, Html Msg )
        notesList =
            Dict.toList notes
                |> List.map Tuple.second
                |> List.sortBy Note.noteOrder
                |> List.map (\note -> noteView
                    { note = note
                    , onInput = WriteNote
                    , onDelete = DeleteNote
                    })
    in
    div [ class "notebook" ]
        [ notebookIdView notebookId clipboardState
        , p [] [ b [] [ text "Warning: " ], text "All notebooks are PUBLIC." ]
        , p [] [ text "Be mindful of what you write here. Never write any personal information, passwords, or any information you want to protect." ]
        , buttonRow
        , Keyed.ul [ class "notesList" ] notesList
        , buttonView { icon = Icons.plus, onClick = AddNote, description = "Add Note" }
        ]


view : Model -> Html Msg
view model =
    let
        notebook : Html Msg
        notebook =
            case model.app of
                OpeningNotebook ->
                    spinner

                NotebookOpen notebookId notes ->
                    openNotebook notebookId notes model.clipboardState
    in
    div [ class "screen" ]
        [ div [ class "notebookScreen" ]
            [ h1 [ class "title" ] [ text "Elm Shared Notes" ]
            , notebook
            ]
        ]
