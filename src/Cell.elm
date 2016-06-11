module Cell exposing (Model, view)


import Board
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Model =
    { id : Int
    , color : String
    }


view : Board.Config -> Model -> Svg a
view boardInfo cell =
    let
        row =
            cell.id // boardInfo.cols

        col =
            cell.id % boardInfo.cols

        x' =
            col * (boardInfo.cellSize + boardInfo.borderSize) + boardInfo.borderSize // 2

        y' =
            row * (boardInfo.cellSize + boardInfo.borderSize) + boardInfo.borderSize // 2
    in
        rect
            [ x (toString x')
            , y (toString y')
            , width <| toString (boardInfo.cellSize + boardInfo.borderSize)
            , height <| toString (boardInfo.cellSize + boardInfo.borderSize)
            , fill cell.color
            , stroke "black"
            , strokeWidth (toString boardInfo.borderSize)
            ]
            []
