module Cell exposing (Model, view, init)

import Board
import Color exposing (Color)
import Color.Convert exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (width, height, fill, stroke, strokeWidth)


type alias Model =
    { id : Int
    , color : Color
    , row : Int
    , col : Int
    , x : Int
    , y : Int
    }


init : Board.Config -> Int -> Color -> Model
init board id color =
    let
        row =
            id // board.cols

        col =
            id % board.cols

        x =
            col * (board.cellSize + board.borderSize) + board.borderSize // 2

        y =
            row * (board.cellSize + board.borderSize) + board.borderSize // 2

    in
        { id = id
        , row = row
        , col = col
        , x = x
        , y = y
        , color = color
        }


view : Board.Config -> Model -> Svg a
view board cell =
    rect
        [ Svg.x (toString cell.x)
        , Svg.y (toString cell.y)
        , width <| toString (board.cellSize + board.borderSize)
        , height <| toString (board.cellSize + board.borderSize)
        , fill <| colorToHex cell.color
        , stroke <| colorToHex Color.darkCharcoal
        , strokeWidth (toString board.borderSize)
        ]
        []

