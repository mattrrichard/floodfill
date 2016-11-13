module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (li, ul)
import Color
import Html.CssHelpers
import String


type CssClasses
    = GameBoard
    | Button
    | ButtonDefault
    | ColorButton GameColor
    | Controls
    | ColorButtonGroup


type GameColor
    = Purple
    | Teal
    | Blue
    | Red
    | Yellow
    | Orange


helpers : Html.CssHelpers.Namespace String id class classList
helpers =
    Html.CssHelpers.withNamespace ""


toColor : GameColor -> Color.Color
toColor label =
    case label of
        Purple ->
            Color.rgb 156 39 176

        Teal ->
            Color.rgb 0 150 136

        Blue ->
            Color.rgb 33 150 243

        Red ->
            Color.rgb 183 28 28

        Yellow ->
            Color.rgb 255 235 59

        Orange ->
            Color.rgb 255 165 0


toCssColor : GameColor -> Color
toCssColor label =
    let
        color =
            toColor label |> Color.toRgb
    in
        rgb color.red color.green color.blue


colors : List GameColor
colors =
    [ Purple, Blue, Teal, Yellow, Red, Orange ]


css : Stylesheet
css =
    stylesheet <|
        [ (.) Button
            [ color (rgb 255 255 255)
            , padding4 (px 6) (px 15) (px 3) (px 15)
            , fontWeight bold
            , borderRadius (px 3)
            , border (px 0)
            , cursor pointer
            , borderBottom3 (px 3) solid transparent
            , outline none
            , margin (px 2)
            , textShadow4 (px 0) (px 1) (px 1) (rgba 0 0 0 0.1)
              -- TODO elm-css does not yet support multiple box shadows
              -- , boxShadow5 (px 0) (px 2) (px 2) (px 0) (rgba 0 0 0 0.14)
              -- , boxShadow5 (px 0) (px 3) (px 1) (px -2) (rgba 0 0 0 0.2)
              -- , boxShadow5 (px 0) (px 1) (px 5) (px 0) (rgba 0 0 0 0.12)
            , property "box-shadow"
                (String.join ","
                    [ "0 2px 2px 0 rgba(0, 0, 0, 0.14)"
                    , "0 3px 1px -2px rgba(0, 0, 0, 0.2)"
                    , "0 1px 5px 0px rgba(0, 0, 0, 0.12)"
                    ]
                )
            , hover
                [ borderBottomColor (rgba 0 0 0 0.25)
                ]
            , active
                [ boxShadow5 inset (px 0) (px 2) (px 6) (rgba 0 0 0 0.35)
                , borderBottomColor transparent
                , paddingBottom (px 6)
                , borderBottomWidth (px 0)
                ]
            ]
        , (.) ButtonDefault
            [ fontWeight (int 600)
            , backgroundColor (hex "#f2f2f2")
            , border3 (px 1) solid (hex "#ccc")
            , padding2 (px 5) (px 15)
            , color (hex "#554559")
            , active [ boxShadow5 inset (px 0) (px 2) (px 6) (rgba 0 0 0 0.15) ]
            , hover [ color (toCssColor Blue) ]
            ]
        , (.) Controls
            [ float left
            ]
        , (.) ColorButtonGroup
            [ width (px 85)
            , overflow hidden
            , paddingLeft (px 0)
            , descendants
                [ li
                    [ float left
                    , listStyleType none
                    ]
                ]
            ]
        ]
            ++ (List.map colorButtonStyle colors)


colorButtonStyle : GameColor -> Snippet
colorButtonStyle color =
    (.) (ColorButton color)
        [ backgroundColor (toCssColor color)
        , width (px 35)
        ]
