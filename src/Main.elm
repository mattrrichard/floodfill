module Main exposing (..)

import Array
import Board
import Cell
import Color exposing (Color)
import Html exposing (..)
import Html.App as Html
import Html.Events as Html
import Html.Attributes exposing (value)
import Random
import Random.Array
import Svg exposing (..)
import Svg.Attributes exposing (..)
import DisjointSet as DSet exposing (DisjointSet)
import DisjointSet.Computation as DSC


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
            Color.rgb 183 28 28

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
            , update = update startupCmd
            , view = view
            , subscriptions = always Sub.none
            }


type alias Model =
    { board : Board.Config
    , cells : List Cell.Model
    , colors : List Color
    , sets : DisjointSet
    , won : Bool
    }


emptyModel : Model
emptyModel =
    { board = Board.init 0 0 0 0
    , cells = []
    , colors = []
    , sets = DSet.init 0
    , won = False
    }

type Msg
    = NewBoard Model
    | ChangeTopleftColor Color
    | Restart


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
            , colors = config.colors
            , sets = DSet.init numCells
            , won = False
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
    in
        gen |> Random.map (Maybe.withDefault Color.black)

colorTopLeftCells : Color -> List Cell.Model -> DSC.Computation (List Cell.Model)
colorTopLeftCells newColor cells =
    let

        colorCell target cell =
            DSC.find cell.id |> DSC.map (\set ->
                if target == set then
                    { cell | color = newColor }
                else
                    cell)

        colorCells target =
            DSC.mapM (colorCell target) cells

        getTargetSet =
            DSC.find 0
    in
        getTargetSet `DSC.andThen` colorCells


testVictory : List Cell.Model -> Int -> DSC.Computation Bool
testVictory cells targetSet =
    case cells of
        x :: xs ->
            DSC.find x.id `DSC.andThen` \set ->
                if set == targetSet then
                    testVictory xs targetSet
                else
                    DSC.return False
        _ ->
            DSC.return True


connectCellsByColor : Board.Config -> List Cell.Model -> DSC.Computation (List Cell.Model)
connectCellsByColor { cols } cells=
    let
        connect cell neighbor =
            if cell.color == neighbor.color then
                DSC.union cell.id neighbor.id
            else
                DSC.return ()

        rows =
            partition cols cells


        northSouth =
            List.map2 connect cells (List.drop cols cells)
                |> DSC.sequence

        eastWest =
            rows
                |> List.map (\row -> List.map2 connect row (List.drop 1 row))
                |> List.concat
                |> DSC.sequence
    in
        northSouth
        |> DSC.andThen (always eastWest)
        |> DSC.map (always cells)


partition : Int -> List a -> List (List a)
partition n xs =
    let
        (left, right) =
            (List.take n xs, List.drop n xs)
    in
        case right of
            [] ->
                [left]

            _ ->
                left :: partition n right



update : Cmd Msg -> Msg -> Model -> ( Model, Cmd Msg )
update restartCmd msg model =
    case msg of
        NewBoard newModel ->
            newModel ! []

        Restart ->
            ( model, restartCmd )

        ChangeTopleftColor newColor ->
            let

                updateColor =
                    colorTopLeftCells newColor model.cells

                updateConnections cells =
                    connectCellsByColor model.board cells

                checkVictory cells =
                    DSC.find 0
                    |> DSC.andThen' (testVictory cells)
                    |> DSC.map ((,) cells)

                ((cells', victory), sets') =
                    updateColor
                    |> DSC.andThen' updateConnections
                    |> DSC.andThen' checkVictory
                    |> DSC.eval' model.sets

                model' =
                    { model | cells = cells'
                            , sets = sets'
                            , won = victory }
            in
                model' ! []


view : Model -> Html Msg
view model =
    div []
        [ div []
              [ Html.text <| if model.won then "You win" else "Keep trying!" ]
        , Svg.svg
            [ width (toString model.board.width)
            , height (toString model.board.height)
            ]
            (List.map (Cell.view model.board) model.cells)
        , ul []
            (List.map colorButton model.colors)
        , div []
            [ input
                  [ type' "button"
                  , value "restart"
                  , Html.onClick Restart
                  ]
                  []
            ]
        ]


colorButton : Color -> Html Msg
colorButton color =
    li []
    [ input
        [ type' "button"
        , value "color button"
        , Html.onClick <| ChangeTopleftColor color
        ]
        []
    ]
