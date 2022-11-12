module AutoTextarea exposing (autoTextarea)

import Css exposing (..)
import Html
import Html.Styled exposing (div, text, textarea)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events


autoExpandStyles : List Style
autoExpandStyles =
    [ position relative
    , width (px 400)
    ]


textareaStyles : List Style
textareaStyles =
    divStyles
        ++ [ position absolute
           , top zero
           , bottom zero
           , width (pct 100)
           , resize none
           ]


divStyles : List Style
divStyles =
    [ padding (px 10)
    , fontSize (px 16)
    , fontFamily sansSerif
    , border3 (px 1) solid (hex "#666")
    , lineHeight (em 1.3)
    , whiteSpace preWrap
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
