module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Svg


type alias Model =
    { boardInfo : BoardInfo
    , cells : List Cell
    }


type alias BoardInfo =
    { cols : Int
    , rows : Int
    , cellSize : Int
    , borderSize : Int
    , width : Int
    , height : Int
    }


type alias Cell =
    { id : Int
    , color : String
    }


type Msg
    = NoOp


init : Int -> Int -> Int -> Int -> Model
init cols rows cellSize borderSize =
    let
        width =
            cols * cellSize + (cols + 1) * borderSize

        height =
            rows * cellSize + (rows + 1) * borderSize

        boardInfo =
            { cols = cols
            , rows = rows
            , cellSize = cellSize
            , borderSize = borderSize
            , width = width
            , height = height
            }

        cells =
            List.map (\id -> { id = id, color = "#ff00ff" }) [0..(cols * rows - 1)]
    in
        { boardInfo = boardInfo
        , cells = cells
        }


main : Program Never
main =
    let
        cols =
            16

        rows =
            16

        cellSize =
            16

        borderSize =
            2

        model =
            init cols rows cellSize borderSize
    in
        Html.program
            { init = ( model, Cmd.none )
            , update = update
            , view = view (cell model.boardInfo)
            , subscriptions = always Sub.none
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


view : (Cell -> Svg Msg) -> Model -> Html Msg
view cellRender model =
    div []
        [ Svg.svg
            [ width (toString model.boardInfo.width)
            , height (toString model.boardInfo.height)
            ]
            (List.map cellRender model.cells)
        ]


cell : BoardInfo -> Cell -> Svg Msg
cell boardInfo cell =
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
