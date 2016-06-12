module Cell exposing (Model, view, init)

import Bitwise
import Board
import Color exposing (Color)
import Color.Convert exposing (..)
import Color.Interpolate exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (width, height, fill, stroke, strokeWidth)


type alias Model =
    { id : Int
    , color : String
    , row : Int
    , col : Int
    , x : Int
    , y : Int
    }


init board id color =
    let
        startColor =
            rgbHex 0x838F
            -- rgbHex 0x1565C0

        endColor =
            -- rgbHex 0xFFFF00
            rgbHex 0x00D50000

        row =
            id // board.cols

        col =
            id % board.cols

        x =
            col * (board.cellSize + board.borderSize) + board.borderSize // 2

        y =
            row * (board.cellSize + board.borderSize) + board.borderSize // 2

        distance =
            sqrt (toFloat <| (row) ^ 2 + (col) ^ 2)

        maxdistance =
            sqrt (toFloat <| (board.rows-1) ^ 2 + (board.cols-1) ^ 2)

        a =
            atan (toFloat (row+ 1) / toFloat (col + 1))

        diagAngle =
            atan (toFloat board.rows / toFloat board.cols)

        t =
            abs (diagAngle - a)

        distance' =
            distance * cos t

        p =
            distance' / maxdistance

        -- p' =
        --     ((p*2 - 1) ^ 3 + 1)/2

        -- p' =
        --     sin (p * pi - pi / 2) / 2 + 0.5

        p' = p

        color' =
            interpolate LAB startColor endColor p'
    in
        { id = id
        , row = row
        , col = col
        , x = x
        , y = y
        , color = colorToHex color'
        }


view : Board.Config -> Model -> Svg a
view board cell =
    rect
        [ Svg.x (toString cell.x)
        , Svg.y (toString cell.y)
        , width <| toString (board.cellSize + board.borderSize)
        , height <| toString (board.cellSize + board.borderSize)
        , fill cell.color
        , stroke cell.color --(colorToCssRgba <| Color.darkGray)
        , strokeWidth (toString board.borderSize)
        ]
        []


rgbHex : Int -> Color.Color
rgbHex hex =
    let
        r =
            Bitwise.and 0xFF <| hex `Bitwise.shiftRight` 16

        g =
            Bitwise.and 0xFF <| hex `Bitwise.shiftRight` 8

        b =
            Bitwise.and 0xFF hex
    in
        Color.rgb r g b
