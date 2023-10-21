port module ClipboardButton exposing (ClipboardState, initClipboardState, ClipboardMsg, updateClipboardState, clipboardButtonView)

import Identifiers exposing (NotebookId)
import Css exposing (..)
import Html.Styled exposing (Html, div, span, text, button)
import Html.Styled.Attributes exposing (css, title, attribute)
import Html.Styled.Events exposing (onClick, onBlur)
import Icons

port copyToClipboard : String -> Cmd msg

type ClipboardState
    = NotCopied
    | Copied

initClipboardState : ClipboardState
initClipboardState = NotCopied

type ClipboardMsg
    = ClickedCopyToClipboard
    | ButtonFocusLost

updateClipboardState : 
    NotebookId 
    -> ClipboardMsg 
    -> ClipboardState
    -> (ClipboardState, Cmd ClipboardMsg)
updateClipboardState notebookId msg _ =
    case msg of
        ClickedCopyToClipboard ->
            ( Copied
            , copyToClipboard (Identifiers.notebookIdToString notebookId)
            )
        
        ButtonFocusLost ->
            ( NotCopied, Cmd.none )

inlineLinkStyles : List Style
inlineLinkStyles =
    [ cursor pointer
    , border2 zero none
    , backgroundColor transparent
    , property "color" "var(--color-text)"
    , property "filter" "drop-shadow(0 0 3px var(--color-action))"
    , hover
        [ property "color" "var(--color-action-hover)"
        , property "filter" "drop-shadow(0 0 3px var(--color-action-hover))"
        ]
    , focus
        [ property "outline" "2px solid var(--color-handle-hover)" ]
    ]

inlineButtonView :
    { icon : List Style -> Html.Styled.Html msg
    , description : String
    , onClickMsg : msg
    , onBlurMsg : msg
    }
    -> Html.Styled.Html msg
inlineButtonView {icon, description, onClickMsg, onBlurMsg} =
    button
        [ css inlineLinkStyles
        , onClick onClickMsg
        , onBlur onBlurMsg
        , title description
        , attribute "aria-label" description
        ]
        [ icon [] ]

clipboardButtonStyles : List Style
clipboardButtonStyles =
    [ displayFlex
    , flexDirection row
    , property "gap" "10px"
    , width (px 20)
    , overflowY visible
    ]

clipboardButtonView : ClipboardState -> Html ClipboardMsg
clipboardButtonView state =
    div
        [ css clipboardButtonStyles ]
        [ inlineButtonView
            { icon = Icons.copy
            , description = "Copy to clipboard"
            , onClickMsg = ClickedCopyToClipboard
            , onBlurMsg = ButtonFocusLost
            }
        , case state of
            NotCopied -> text ""
            Copied -> span [] [ text "Copied!" ]
        ]