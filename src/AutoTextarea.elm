module AutoTextarea exposing (autoTextarea)

import Css exposing (..)
import Html.Styled exposing (Html, button, div, text, textarea)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Icons exposing (iconButtonStyles, trash2)


marginForButtons : Px
marginForButtons =
    px 40


autoExpandStyles : List Style
autoExpandStyles =
    [ position relative
    , width (pct 100)
    , borderRadius (px 2)
    , margin2 (px 2) zero
    , display inlineFlex
    , property "border-left" "2px solid var(--color-handle)"
    , property "--delete-button-visibility" "hidden"
    , hover
        [ property "border-left" "2px solid var(--color-handle-hover)"
        , property
            "background"
            "linear-gradient(90deg, transparent, 70%, transparent, 95%, var(--color-handle))"
        , property "--delete-button-visibility" "visible"
        ]
    , pseudoClass "focus-within"
        [ margin zero
        , property "border" "2px solid var(--color-handle-hover)"
        , property "--delete-button-visibility" "visible"
        ]
    ]


textareaStyles : List Style
textareaStyles =
    divStyles
        ++ [ position absolute
           , height (pct 100)
           , top zero
           , bottom zero
           , left zero
           , resize none
           , property "color" "var(--color-text)"
           , outline none
           ]


divStyles : List Style
divStyles =
    [ padding (px 10)
    , fontSize (px 16)
    , fontFamily sansSerif
    , border (px 0)
    , lineHeight (em 1.3)
    , whiteSpace preWrap
    , color transparent
    , backgroundColor transparent
    , margin zero
    , textAlign left
    , width (calc (pct 100) minus marginForButtons)
    ]


deleteButtonStyles : List Style
deleteButtonStyles =
    iconButtonStyles
        ++ [ backgroundColor transparent
           , padding zero
           , width marginForButtons
           , property "visibility" "var(--delete-button-visibility)"
           , property "filter" "drop-shadow(0 0 3px var(--color-action))"
           ]


autoTextarea :
    { value : String
    , onInput : String -> msg
    , onDelete : msg
    , placeholder : String
    }
    -> Html msg
autoTextarea { value, onInput, onDelete, placeholder } =
    div [ css autoExpandStyles ]
        [ textarea
            [ css textareaStyles
            , Events.onInput onInput
            , Attributes.placeholder placeholder
            ]
            [ text value ]
        , div
            [ css divStyles
            , Attributes.attribute "aria-hidden" "true"
            , Attributes.tabindex -1
            ]
            [ text (value ++ "_") ]
        , button
            [ css deleteButtonStyles
            , Events.onClick onDelete
            , Attributes.attribute "aria-label" "Delete note"
            , Attributes.title "Delete note"
            ]
            [ trash2 ]
        ]
