module Styles exposing (..)

import Css exposing (..)
import Color


type Classes
    = GameBoard
    | ColorButton


type GameColor
    = Purple
    | Teal
    | Blue
    | Red
    | Yellow


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


colors : List GameColor
colors =
    [ Purple, Blue, Teal, Red, Yellow ]
