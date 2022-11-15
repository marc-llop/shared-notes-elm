module Spinner exposing (spinner)

import Css exposing (..)
import Css.Animations as Anim exposing (Keyframes, keyframes)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)


colorText : String
colorText =
    "f7d29f"


rippleStyles : List Style
rippleStyles =
    [ display inlineBlock
    , position relative
    , width (px 80)
    , height (px 80)
    , boxSizing contentBox
    ]


rippleChildStyles : List Style
rippleChildStyles =
    [ position absolute
    , border3 (px 4) solid (hex colorText)
    , opacity (num 1)
    , borderRadius (pct 50)
    , animationName rippleAnimation
    , animationDuration (sec 1)
    , property "animation-timing-function" "cubic-bezier(0, 0.2, 0.8, 1)"
    , animationIterationCount Css.infinite
    , boxSizing contentBox
    ]


rippleChild2Styles : List Style
rippleChild2Styles =
    rippleChildStyles
        ++ [ animationDelay (sec -0.5)
           ]


rippleAnimation : Keyframes {}
rippleAnimation =
    keyframes
        [ ( 0
          , [ Anim.property "top" "36px"
            , Anim.property "left" "36px"
            , Anim.property "width" "0"
            , Anim.property "height" "0"
            , Anim.opacity zero
            ]
          )
        , ( 4
          , [ Anim.property "top" "36px"
            , Anim.property "left" "36px"
            , Anim.property "width" "0"
            , Anim.property "height" "0"
            , Anim.opacity zero
            ]
          )
        , ( 5
          , [ Anim.property "top" "36px"
            , Anim.property "left" "36px"
            , Anim.property "width" "0"
            , Anim.property "height" "0"
            , Anim.opacity (num 1)
            ]
          )
        , ( 100
          , [ Anim.property "top" "0px"
            , Anim.property "left" "0px"
            , Anim.property "width" "72px"
            , Anim.property "height" "72px"
            , Anim.opacity zero
            ]
          )
        ]


spinner : Html msg
spinner =
    div [ css rippleStyles ]
        [ div [ css rippleChildStyles ] []
        , div [ css rippleChild2Styles ] []
        ]
