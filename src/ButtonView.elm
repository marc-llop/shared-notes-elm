module ButtonView exposing (buttonLinkView, buttonView)

import Css exposing (..)
import Html.Styled exposing (Html, a, button, div)
import Html.Styled.Attributes as Attributes exposing (attribute, css, title)
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


buttonView :
    { icon : List Style -> Html.Styled.Html msg
    , onClick : msg
    , description : String
    }
    -> Html msg
buttonView { icon, onClick, description } =
    div
        [ css divStyles ]
        [ button
            [ css buttonStyles
            , Events.onClick onClick
            , attribute "aria-label" description
            , title description
            ]
            [ icon iconStyles ]
        ]


linkStyles : List Style
linkStyles =
    [ marginTop (px 16)
    , width (px 40)
    ]


buttonLinkView :
    { icon : List Style -> Html.Styled.Html msg
    , href : String
    , description : String
    }
    -> Html.Styled.Html msg
buttonLinkView { icon, href, description } =
    div
        [ css (divStyles ++ linkStyles) ]
        [ a
            [ css buttonStyles
            , Attributes.href href
            , attribute "aria-label" description
            , title description
            ]
            [ icon iconStyles ]
        ]
