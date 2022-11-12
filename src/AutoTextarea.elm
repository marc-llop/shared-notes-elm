module AutoTextarea exposing (autoTextarea)

import Css exposing (..)
import Html
import Html.Styled exposing (div, text, textarea)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events


autoExpandStyles =
    [ position relative
    , width (px 400)
    ]


textareaStyles =
    divStyles
        ++ [ position absolute
           , top zero
           , bottom zero
           , width (pct 100)
           , resize none
           ]


divStyles =
    [ padding (px 10)
    , fontSize (px 16)
    , fontFamily sansSerif
    , border3 (px 1) solid (hex "#666")
    , lineHeight (em 1.3)
    , whiteSpace preWrap
    ]


autoTextarea :
    { text : String
    , onInput : String -> msg
    , placeholder : String
    }
    -> Html.Html msg
autoTextarea { text, onInput, placeholder } =
    div [ css autoExpandStyles ]
        [ textarea
            [ css textareaStyles
            , Events.onInput onInput
            , Attributes.placeholder placeholder
            ]
            [ Html.Styled.text text ]
        , div [ css divStyles ] [ Html.Styled.text (text ++ "_") ]
        ]
        |> Html.Styled.toUnstyled
