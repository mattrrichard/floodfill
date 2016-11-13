module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (li, ul)
import Color
import Html.CssHelpers


type CssClasses
    = GameBoard
    | Button
    | ButtonDefault
    | ColorButton GameColor
    | Controls


type GameColor
    = Purple
    | Teal
    | Blue
    | Red
    | Yellow


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


toCssColor : GameColor -> Color
toCssColor label =
    let
        color =
            toColor label |> Color.toRgb
    in
        rgb color.red color.green color.blue


colors : List GameColor
colors =
    [ Purple, Blue, Teal, Red, Yellow ]


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
            , margin (px 5)
            , textShadow4 (px 0) (px 1) (px 1) (rgba 0 0 0 0.1)
            , hover [ borderBottomColor (rgba 0 0 0 0.25) ]
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
            , descendants
                [ li
                    [ listStyleType none
                    ]
                , ul
                    [ paddingLeft (px 0) ]
                ]
            ]
        ]
            ++ (List.map colorButtonStyle colors)


colorButtonStyle : GameColor -> Snippet
colorButtonStyle color =
    ColorButton color . [ backgroundColor (toCssColor color) ]
