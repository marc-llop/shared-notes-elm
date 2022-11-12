module AutoTextarea exposing (autoTextarea)

import Css exposing (..)
import Html
import Html.Styled exposing (div, text, textarea)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events


autoExpandStyles : List Style
autoExpandStyles =
    [ position relative
    , width (pct 100)
    ]


textareaStyles : List Style
textareaStyles =
    divStyles
        ++ [ position absolute
           , top zero
           , bottom zero
           , left zero
           , boxSizing borderBox
           , width (pct 100)
           , resize none
           , property "color" "var(--color-text)"
           , property "border-left" "2px solid var(--color-handle)"
           , hover
                [ property
                    "background"
                    "linear-gradient(90deg, transparent, 70%, transparent, 95%, var(--color-handle))"
                ]
           , focus
                [ borderLeft3 (px 2) solid transparent
                , property "outline" "2px groove var(--color-handle)"
                ]
           ]


divStyles : List Style
divStyles =
    [ padding (px 10)
    , fontSize (px 16)
    , fontFamily sansSerif
    , borderTop (px 0)
    , borderBottom (px 0)
    , borderRight (px 0)
    , lineHeight (em 1.3)
    , whiteSpace preWrap
    , color transparent
    , backgroundColor transparent
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
        ]
        |> Html.Styled.toUnstyled
