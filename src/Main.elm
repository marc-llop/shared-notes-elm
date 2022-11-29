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
import Identifiers exposing (NotebookId, generateShortId, notebookIdToString, parseNotebookId)
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
        firstSeed : Random.Seed
        firstSeed =
            Random.initialSeed randomSeed

        newNotebook : ( App, Cmd Msg )
        newNotebook =
            ( OpeningNewNotebook
            , Identifiers.generateNotebookId IdGenerated firstSeed
            )

        existingNotebook : NotebookId -> ( App, Cmd Msg )
        existingNotebook notebookId =
            ( NotebookOpen notebookId exampleNotes
            , Cmd.none
            )

        newApp : ( App, Cmd Msg )
        newApp =
            case String.toList path of
                [ '/' ] ->
                    newNotebook

                '/' :: notebookId ->
                    String.fromList notebookId
                        |> parseNotebookId
                        |> Result.map existingNotebook
                        |> Result.withDefault newNotebook

                _ ->
                    newNotebook
    in
    Tuple.mapFirst
        (\app -> { randomSeed = firstSeed, app = app })
        newApp


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = \model -> view model |> Html.Styled.toUnstyled
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = IdGenerated NotebookId Random.Seed
    | NotebookStored (Result Http.Error NotebookId)
    | WriteNote String String
    | AddNote
    | NoteCreated ClientOnlyNote Random.Seed
    | NoteStored ClientOnlyNote (Result Http.Error StoredNote)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IdGenerated notebookId seed ->
            ( { app = NotebookOpen notebookId exampleNotes
              , randomSeed = seed
              }
            , Cmd.batch
                [ updateLocation (notebookIdToString notebookId)
                , Task.attempt NotebookStored (insertNotebook notebookId)
                ]
            )

        NotebookStored _ ->
            ( model, Cmd.none )

        WriteNote noteId value ->
            ( { model
                | app =
                    case model.app of
                        OpeningNewNotebook ->
                            model.app

                        NotebookOpen notebookId notes ->
                            let
                                updateMaybeNote =
                                    Maybe.map (Note.updateNoteText value)
                            in
                            NotebookOpen notebookId
                                (Dict.update noteId updateMaybeNote notes)
              }
            , Cmd.none
            )

        AddNote ->
            ( model, Note.newNote NoteCreated model.randomSeed )

        NoteCreated note newSeed ->
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
                    , Task.attempt (NoteStored note) (Note.insertNote nId note)
                    )

        NoteStored oldNote result ->
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
        [ span [] [ text <| notebookIdToString notebookId ]
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
