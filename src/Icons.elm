module Icons exposing
    ( github
    , iconButtonStyles
    , plus
    , trash2
    )

import Css exposing (..)
import Html.Styled exposing (Html)
import Html.Styled.Attributes
import Svg.Styled exposing (Svg, svg)
import Svg.Styled.Attributes as Attr exposing (d, points, x1, x2, y1, y2)


iconButtonStyles : List Style
iconButtonStyles =
    [ cursor pointer
    , border zero
    , property "color" "var(--color-action)"
    , displayFlex
    , alignItems center
    , justifyContent center
    , hover
        [ property "color" "var(--color-action-hover)"
        ]
    , focus
        [ property "outline" "2px solid var(--color-handle-hover)" ]
    ]


svgFeatherIcon : String -> List Style -> List (Svg msg) -> Html msg
svgFeatherIcon iconName extraStyles =
    svg
        [ Attr.class <| "feather feather-" ++ iconName
        , Attr.fill "none"
        , Attr.height "20"
        , Attr.stroke "currentColor"
        , Attr.strokeLinecap "round"
        , Attr.strokeLinejoin "round"
        , Attr.strokeWidth "2"
        , Attr.viewBox "0 0 24 24"
        , Attr.width "20"
        , Html.Styled.Attributes.css extraStyles
        ]


github : Html msg
github =
    svgFeatherIcon "github"
        []
        [ Svg.Styled.path [ d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" ] []
        ]


plus : List Style -> Html msg
plus styles =
    svgFeatherIcon "plus"
        styles
        [ Svg.Styled.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.Styled.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


trash2 : Html msg
trash2 =
    svgFeatherIcon "trash-2"
        []
        [ Svg.Styled.polyline [ points "3 6 5 6 21 6" ] []
        , Svg.Styled.path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
        , Svg.Styled.line [ x1 "10", y1 "11", x2 "10", y2 "17" ] []
        , Svg.Styled.line [ x1 "14", y1 "11", x2 "14", y2 "17" ] []
        ]
