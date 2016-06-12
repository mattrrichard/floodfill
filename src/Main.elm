module Main exposing (..)


import Board
import Cell
import Html exposing (..)
import Html.App as Html
import Html.Events as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Svg

type alias Model =
    { board : Board.Config
    , cells : List Cell.Model
    }



type Msg
    = NoOp


init : Int -> Int -> Int -> Int -> Model
init rows cols cellSize borderSize =
    let
        board =
            Board.init rows cols cellSize borderSize

        cells =
            List.map (\id -> { id = id, color = "#ff00ff" }) [0..(cols * rows - 1)]
    in
        { board = board
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
            , view = view (Cell.view model.board)
            , subscriptions = always Sub.none
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


view : (Cell.Model -> Svg Msg) -> Model -> Html Msg
view cellView model =
    div []
        [ Svg.svg
            [ width (toString model.board.width)
            , height (toString model.board.height)
            ]
            (List.map cellView model.cells)
        ]
