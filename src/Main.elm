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
    = OpeningNewNotebook
    | NotebookOpen NotebookId Notes


type alias Model =
    { randomSeed : Random.Seed
    , app : App
    }


exampleNotes : Notes
exampleNotes =
    Note.exampleNotes
        |> List.map noteToPair
        |> Dict.fromList


type alias Flags =
    { path : String, randomSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init { path, randomSeed } =
    let
        newNotebook : ( App, Cmd Msg )
        newNotebook =
            ( OpeningNewNotebook
            , Identifiers.fetchTwoWords WordsFetched
            )

        existingNotebook : NotebookId -> ( App, Cmd Msg )
        existingNotebook notebookId =
            ( OpeningNewNotebook
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
    | NotebookStored (Result Http.Error NotebookId)
    | WriteNote String String
    | AddNote
    | NoteStored ClientOnlyNote (Result Http.Error StoredNote)
    | NoteUpdated StoredNote (Result Http.Error StoredNote)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                    ( { model | app = NotebookOpen notebookId exampleNotes }
                    , Cmd.none
                      --TODO fetch notes
                    )

                Err _ ->
                    ( model, Identifiers.fetchTwoWords WordsFetched )

        NotebookStored _ ->
            ( model, Cmd.none )

        WriteNote noteId value ->
            case model.app of
                OpeningNewNotebook ->
                    ( model, Cmd.none )

                NotebookOpen notebookId notes ->
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
                            Note.patchNote (NoteUpdated note) notebookId note
                    )

        NoteUpdated oldNote result ->
            ( model, Cmd.none )

        AddNote ->
            let
                ( note, newSeed ) =
                    Note.newNote model.randomSeed
            in
            case model.app of
                OpeningNewNotebook ->
                    ( model, Cmd.none )

                NotebookOpen nId notes ->
                    let
                        newNote : Note
                        newNote =
                            ClientOnly note

                        noteId : String
                        noteId =
                            Note.noteIdString newNote

                        newApp : App
                        newApp =
                            NotebookOpen nId (Dict.insert noteId newNote notes)
                    in
                    ( { randomSeed = newSeed, app = newApp }
                    , Note.insertNewNote (NoteStored note) nId
                    )

        NoteStored oldNote result ->
            updateStoredNoteIdInModel oldNote result model


updateStoredNoteIdInModel : ClientOnlyNote -> Result Http.Error StoredNote -> Model -> ( Model, Cmd Msg )
updateStoredNoteIdInModel oldNote result model =
    case ( result, model.app ) of
        ( _, OpeningNewNotebook ) ->
            ( model, Cmd.none )

        ( Ok storedNote, NotebookOpen nId notes ) ->
            ( { model
                | app = NotebookOpen nId (changeNoteId oldNote storedNote notes)
              }
            , Cmd.none
            )

        ( Err _, NotebookOpen _ _ ) ->
            ( model, Cmd.none )


openNotebookInModel : Random.Seed -> NotebookId -> ( Model, Cmd Msg )
openNotebookInModel newSeed notebookId =
    ( { randomSeed = newSeed
      , app = NotebookOpen notebookId exampleNotes
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
    openNotebookInModel newSeed notebookId


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
                OpeningNewNotebook ->
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
