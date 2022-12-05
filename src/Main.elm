port module Main exposing (Flags, Model, Msg, main)

-- import VitePluginHelper

import AutoTextarea exposing (autoTextarea)
import Browser
import ButtonView exposing (buttonLinkView, buttonView)
import Css
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Html, b, div, h1, p, span, text, toUnstyled)
import Html.Styled.Attributes as Attributes exposing (class)
import Html.Styled.Keyed as Keyed
import Http
import Icons
import Identifiers exposing (NotebookId)
import Note exposing (ClientOnlyNote, Note(..), StoredNote, noteIdString, noteToPair, noteView)
import Notebook exposing (insertNotebook)
import Random
import Spinner exposing (spinner)
import Task


port updateLocation : String -> Cmd msg


type alias Notes =
    Dict String Note


type App
    = OpeningNotebook
    | NotebookOpen NotebookId Notes


type alias Model =
    { randomSeed : Random.Seed
    , app : App
    }


dictFromNotes : List StoredNote -> Notes
dictFromNotes noteList =
    List.map Stored noteList
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
    | NotebookFetched NotebookId (Result Http.Error ( List StoredNote, Random.Seed ))
    | NotebookStored (Result Http.Error NotebookId)
    | WriteNote String String
    | AddNote
    | NoteStored ClientOnlyNote (Result Http.Error ( StoredNote, Random.Seed ))
    | NoteUpdated StoredNote (Result Http.Error ( StoredNote, Random.Seed ))


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

        WriteNote _ _ ->
            ( model, Cmd.none )

        AddNote ->
            ( model, Cmd.none )

        NoteStored _ _ ->
            ( model, Cmd.none )

        NoteUpdated _ _ ->
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

        WriteNote noteId value ->
            let
                maybeNewNote : Maybe Note
                maybeNewNote =
                    Dict.get noteId notes
                        |> Maybe.map (Note.updateNoteText value)

                updateInDict : Maybe Note -> Maybe Note
                updateInDict =
                    Maybe.andThen (\_ -> maybeNewNote)
            in
            ( { model
                | app =
                    NotebookOpen notebookId
                        (Dict.update noteId updateInDict notes)
              }
            , case maybeNewNote of
                Nothing ->
                    Cmd.none

                Just (ClientOnly note) ->
                    Cmd.none

                Just (Stored note) ->
                    Note.patchNote model.randomSeed (NoteUpdated note) notebookId note
            )

        AddNote ->
            let
                ( note, newSeed ) =
                    Note.newNote model.randomSeed

                newNote : Note
                newNote =
                    ClientOnly note

                noteId : String
                noteId =
                    Note.noteIdString newNote

                newApp : App
                newApp =
                    NotebookOpen notebookId (Dict.insert noteId newNote notes)
            in
            ( { randomSeed = newSeed, app = newApp }
            , Note.insertNewNote model.randomSeed (NoteStored note) notebookId
            )

        NoteStored oldNote result ->
            case result of
                Ok ( storedNote, newSeed ) ->
                    ( { model
                        | app = NotebookOpen notebookId (changeNoteId oldNote storedNote notes)
                        , randomSeed = newSeed
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


openNewNotebookInModel : Random.Seed -> NotebookId -> ( Model, Cmd Msg )
openNewNotebookInModel newSeed notebookId =
    ( { randomSeed = newSeed
      , app = NotebookOpen notebookId Dict.empty
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


changeNoteId : ClientOnlyNote -> StoredNote -> Dict String Note -> Dict String Note
changeNoteId oldNote storedNote notes =
    let
        note : Note
        note =
            Stored storedNote

        oldId : String
        oldId =
            noteIdString (ClientOnly oldNote)
    in
    Dict.remove oldId notes
        |> Dict.insert (noteIdString note) note


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


openNotebook : NotebookId -> Notes -> Html Msg
openNotebook notebookId notes =
    let
        notesList : List ( String, Html Msg )
        notesList =
            Dict.toList notes
                |> List.sortBy (Tuple.first >> String.toInt >> Maybe.withDefault 0)
                |> List.map (\( _, note ) -> noteView { note = note, onInput = WriteNote })
    in
    div [ class "notebook" ]
        [ span [] [ text <| Identifiers.notebookIdToString notebookId ]
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
                    openNotebook notebookId notes
    in
    div [ class "screen" ]
        [ div [ class "notebookScreen" ]
            [ h1 [ class "title" ] [ text "Elm Shared Notes" ]
            , notebook
            ]
        ]
