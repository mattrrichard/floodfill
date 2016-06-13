module Main exposing (..)

import Array
import Board
import Cell
import Color exposing (Color)
import Html exposing (..)
import Html.App as Html
import Html.Events as Html
import Random
import Random.Array
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events as Svg


type alias Config =
    { rows : Int
    , cols : Int
    , cellSize : Int
    , borderSize : Int
    , colors : List Color
    }


main : Program Never
main =
    let
        purple =
            Color.rgb 156 39 176

        blue =
            Color.rgb 33 150 243

        teal =
            Color.rgb 0 150 136

        red =
            Color.rgb 244 67 54

        yellow =
            Color.rgb 255 235 59

        config =
            { rows = 16
            , cols = 16
            , cellSize = 24
            , borderSize = 2
            , colors = [ purple, blue, teal, red, yellow ]
            }

        model =
            emptyModel

        startupCmd =
            Random.generate NewBoard (init config)
    in
        Html.program
            { init = ( model, startupCmd )
            , update = update
            , view = view
            , subscriptions = always Sub.none
            }


type alias Model =
    { board : Board.Config
    , cells : List Cell.Model
    }


emptyModel : Model
emptyModel =
    { board = Board.init 0 0 0 0
    , cells = []
    }


type Msg
    = NewBoard Model


init : Config -> Random.Generator Model
init config =
    let
        board =
            Board.init config.rows config.cols config.cellSize config.borderSize

        numCells =
            config.cols * config.rows

        ids =
            [0..numCells - 1]

        colorsGen =
            Random.list numCells (randomColor config.colors)

        cellsGen =
            Random.map (List.map2 (Cell.init board) ids) colorsGen

        model cells =
            { board = board
            , cells = cells
            }
    in
        Random.map model cellsGen


randomColor : List Color -> Random.Generator Color
randomColor colors =
    let
        arr =
            Array.fromList colors

        gen =
            Random.Array.sample arr

        unwrap c =
            case c of
                Just color ->
                    color

                --should never happen. using black to make it stand out
                Nothing ->
                    Color.black
    in
        Random.map unwrap gen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewBoard newModel ->
            newModel ! []


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg
            [ width (toString model.board.width)
            , height (toString model.board.height)
            ]
            (List.map (Cell.view model.board) model.cells)
        ]
