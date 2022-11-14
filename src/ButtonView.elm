module ButtonView exposing (buttonView)

import Css exposing (..)
import Html exposing (Html)
import Html.Styled exposing (button, div, toUnstyled)
import Html.Styled.Attributes exposing (attribute, css, title)
import Html.Styled.Events as Events
import Icons exposing (iconButtonStyles)


divStyles : List Style
divStyles =
    [ displayFlex
    , width (pct 100)
    , justifyContent center
    ]


buttonStyles : List Style
buttonStyles =
    iconButtonStyles
        ++ [ displayFlex
           , width (px 150)
           , height (px 40)
           , borderRadius (px 2)
           , property "background-color" "var(--color-handle)"
           , marginBottom (px 30)
           , property "filter" "none"
           , property "--add-note-icon-filter" "none"
           , hover
                [ property "outline" "2px solid var(--color-handle-hover)"
                , property
                    "--add-note-icon-filter"
                    ""
                ]
           , active
                [ boxShadow2 zero zero
                , outline zero
                ]
           ]


iconStyles : List Style
iconStyles =
    [ property "filter" "drop-shadow(0 0 3px var(--color-action))"
    ]


buttonView : {icon : List Style -> Html.Styled.Html msg, onClick : msg} -> Html msg
buttonView {icon, onClick} =
    div
        [ css divStyles ]
        [ button
            [ css buttonStyles
            , Events.onClick onClick
            ]
            [ icon iconStyles ]
        ]
        |> toUnstyled
