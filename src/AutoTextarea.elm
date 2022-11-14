module AutoTextarea exposing (autoTextarea)

import Css exposing (..)
import Html
import Html.Styled exposing (div, text, textarea, button)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Icons exposing (trash2)

marginForButtons = (px 40)

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
    [ backgroundColor transparent
    , padding zero
    , width marginForButtons
    , border zero
    , cursor pointer
    , property "visibility" "var(--delete-button-visibility)"
    , property "color" "var(--color-action)"
    , displayFlex
    , alignItems center
    , justifyContent center
    , hover
        [ property "filter" "drop-shadow(0 0 3px var(--color-action))"
        , property "color" "var(--color-action-hover)"
        ]
    , focus
        [ property "outline" "2px solid var(--color-handle-hover)" ]
    ]

trashIconStyles : List Style
trashIconStyles =
    [ width (px 20)
    ]


autoTextarea :
    { value : String
    , onInput : String -> msg
    , placeholder : String
    }
    -> Html.Html msg
autoTextarea { value, onInput, placeholder } =
    div [ css autoExpandStyles ]
        [ textarea
            [ css textareaStyles
            , Events.onInput onInput
            , Attributes.placeholder placeholder
            ]
            [ text value ]
        , div [ css divStyles ] [ text (value ++ "_") ]
        , button
            [ css deleteButtonStyles
            , Events.onClick (onInput value)
            , Attributes.attribute "aria-label" "Delete note"
            , Attributes.title "Delete note"
            ]
            [ trash2 trashIconStyles ] 
        ]
        |> Html.Styled.toUnstyled
