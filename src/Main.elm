port module Main exposing (Flags, Model, Msg, main)

import Browser
import ClipboardButton
import Html.Styled exposing (Html, b, div, h1, text)
import Html.Styled.Attributes exposing (class)
import Http
import Identifiers exposing (NotebookId, ValidWord)
import Note exposing (Note)
import Notebook exposing (insertNotebook)
import OpenNotebook
import Random
import Spinner exposing (spinner)
import Supabase exposing (CallError(..))


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
    | OpenNotebook OpenNotebook.Model



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


initialClientId : Int
initialClientId =
    0



----- UPDATE -----


{-| Messages only sent during application initialization.
-}
type InitializationMsg
    = NotebookChecked (Result CallError NotebookId)
    | WordsFetched (Result Http.Error ( ValidWord, ValidWord ))
    | NotebookFetched NotebookId (Result CallError ( List Note, Int ))


type Msg
    = Initializing InitializationMsg
    | Initialized OpenNotebook.Msg


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
                    OpenNotebook.update appMsg loadedModel
                        |> Tuple.mapBoth OpenNotebook (Cmd.map Initialized)


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
                    Identifiers.fullyRandomNotebookId randomSeed
                        |> Tuple.second
                        |> openNewNotebookOffline
            in
            case result of
                Ok notebookId ->
                    ( model
                    , Notebook.getNotebookNotes initialClientId (Initializing << NotebookFetched notebookId) notebookId
                    )

                Err DataError ->
                    networkOkButNotebookNotFound

                Err ConnectionError ->
                    networkFailed

        WordsFetched result ->
            case result of
                Ok ( a, b ) ->
                    Identifiers.randomNotebookIdWithWords ( a, b ) randomSeed
                        |> Tuple.second
                        |> openNewNotebookOnline

                Err _ ->
                    Identifiers.fullyRandomNotebookId randomSeed
                        |> Tuple.second
                        |> openNewNotebookOnline

        NotebookFetched notebookId result ->
            case result of
                Ok ( notes, newSmallestAvailableId ) ->
                    openExistingNotebookOnline notes notebookId newSmallestAvailableId

                Err _ ->
                    openExistingNotebookOffline notebookId


openExistingNotebookOnline : List Note -> NotebookId -> Int -> ( Model, Cmd Msg )
openExistingNotebookOnline notes notebookId smallestAvailableId =
    ( OpenNotebook
        { smallestAvailableId = smallestAvailableId
        , connectionStatus = OpenNotebook.NotebookOnline
        , notebookId = notebookId
        , notes = OpenNotebook.noteListToNotes notes
        , clipboardState = ClipboardButton.initClipboardState
        }
    , Cmd.none
    )


openExistingNotebookOffline : NotebookId -> ( Model, Cmd Msg )
openExistingNotebookOffline notebookId =
    ( OpenNotebook
        { smallestAvailableId = initialClientId
        , connectionStatus = OpenNotebook.NotebookOffline []
        , notebookId = notebookId
        , notes = OpenNotebook.emptyNotes
        , clipboardState = ClipboardButton.initClipboardState
        }
    , Cmd.none
    )


{-| Updates the notebook to reflect the notebook is open.
Updates the location and tries to store the notebook.
-}
openNewNotebookOnline : NotebookId -> ( Model, Cmd Msg )
openNewNotebookOnline notebookId =
    ( OpenNotebook
        { smallestAvailableId = initialClientId
        , connectionStatus = OpenNotebook.NotebookNotStored
        , notebookId = notebookId
        , notes = OpenNotebook.emptyNotes
        , clipboardState = ClipboardButton.initClipboardState
        }
    , Cmd.batch
        [ updateLocation (Identifiers.notebookIdToString notebookId)
        , insertNotebook (Initialized << OpenNotebook.NotebookStored) notebookId
        ]
    )


{-| Updates the model to reflect the notebook is open.
Does not update the URL, does not try to store the notebook.
-}
openNewNotebookOffline : NotebookId -> ( Model, Cmd Msg )
openNewNotebookOffline notebookId =
    ( OpenNotebook
        { smallestAvailableId = initialClientId
        , connectionStatus = OpenNotebook.NotebookNotStored
        , notebookId = notebookId
        , notes = OpenNotebook.emptyNotes
        , clipboardState = ClipboardButton.initClipboardState
        }
    , Cmd.none
    )



----- VIEW -----


view : Model -> Html Msg
view model =
    let
        notebook : Html Msg
        notebook =
            case model of
                OpeningNotebook _ ->
                    spinner

                OpenNotebook loadedModel ->
                    OpenNotebook.view loadedModel
                        |> Html.Styled.map Initialized
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
        , view = view >> Html.Styled.toUnstyled
        , subscriptions = \_ -> Sub.none
        }
