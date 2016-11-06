module Main exposing (..)

import Array
import Board
import Cell
import Color exposing (Color)
import DisjointSet as DSet exposing (DisjointSet)
import DisjointSet.Computation as DSC
import Html exposing (..)
import Html.App as Html
import Html.Attributes as Html exposing (value)
import Html.Events as Html
import Html.Events.Extra as Html
import Json.Decode as Json
import Keyboard
import Konami as Code
import Random
import Random.Array
import Styles as Style
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)


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
        config =
            { rows = 16
            , cols = 16
            , cellSize = 24
            , borderSize = 2
            , colors = Style.colors |> List.map Style.toColor
            }

        startupCmd =
            Random.generate NewBoard (init config emptyModel)

        discoCmd =
            Random.generate NewBoard << init config
    in
        Html.program
            { init = ( emptyModel, startupCmd )
            , update = update startupCmd discoCmd
            , view = view
            , subscriptions = subscriptions
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.disco then
        Time.every (model.discoTickDuration * Time.millisecond) (always DiscoTick)
    else
        Keyboard.downs KeyPress


type alias Model =
    { board : Board.Config
    , cells : List Cell.Model
    , colors : List Color
    , sets : DisjointSet
    , won : Bool
    , disco : Bool
    , discoTickDuration : Float
    , codeState : Code.State
    }


emptyModel : Model
emptyModel =
    { board = Board.init 0 0 0 0
    , cells = []
    , colors = []
    , sets = DSet.init 0
    , won = False
    , disco = False
    , discoTickDuration = 50
    , codeState = Code.init
    }


init : Config -> Model -> Random.Generator Model
init config model =
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

        newModelFactory cells =
            { board = board
            , cells = cells
            , colors = config.colors
            , sets = DSet.init numCells
            , won = False
            , disco = model.disco
            , discoTickDuration = model.discoTickDuration
            , codeState = Code.init
            }
    in
        Random.map newModelFactory cellsGen


randomColor : List Color -> Random.Generator Color
randomColor colors =
    let
        arr =
            Array.fromList colors

        gen =
            Random.Array.sample arr
    in
        gen |> Random.map (Maybe.withDefault Color.black)


type Msg
    = NewBoard Model
    | ChangeTopleftColor Color
    | Restart
    | DiscoTick
    | ChangeTickDuration Float
    | KeyPress Int


update : Cmd Msg -> (Model -> Cmd Msg) -> Msg -> Model -> ( Model, Cmd Msg )
update restartCmd discoCmd msg model =
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

                ( cells, sets ) =
                    updateColor
                        |> DSC.andThen updateConnections
                        |> DSC.eval model.sets

                newModel =
                    { model
                        | cells = cells
                        , sets = sets
                        , won = testVictory cells
                    }
            in
                newModel ! []

        DiscoTick ->
            ( model, discoCmd model )

        ChangeTickDuration tick ->
            { model | discoTickDuration = tick } ! []

        KeyPress code ->
            let
                ( codeState, triggered ) =
                    Code.update code model.codeState
            in
                { model
                    | codeState = codeState
                    , disco = model.disco || triggered
                }
                    ! []


colorTopLeftCells : Color -> List Cell.Model -> DSC.Computation (List Cell.Model)
colorTopLeftCells newColor cells =
    let
        colorCell target cell =
            DSC.find cell.id
                |> DSC.map
                    (\set ->
                        if target == set then
                            { cell | color = newColor }
                        else
                            cell
                    )

        colorCells target =
            DSC.mapM (colorCell target) cells

        getTargetSet =
            DSC.find 0
    in
        getTargetSet
            |> DSC.andThen colorCells


testVictory : List (Cell.Model) -> Bool
testVictory cells =
    case cells of
        x :: xs ->
            xs
                |> List.map .color
                |> List.all ((==) x.color)

        _ ->
            True


connectCellsByColor : Board.Config -> List Cell.Model -> DSC.Computation (List Cell.Model)
connectCellsByColor { cols } cells =
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
        ( left, right ) =
            ( List.take n xs, List.drop n xs )
    in
        case right of
            [] ->
                [ left ]

            _ ->
                left :: partition n right


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ Html.text
                <| if model.disco then
                    ""
                   else if model.won then
                    "You win"
                   else
                    "Keep trying!"
            ]
        , Svg.svg
            [ width (toString model.board.width)
            , height (toString model.board.height)
            ]
            (List.map (Cell.view model.board) model.cells)
        , if not model.disco then
            ul []
                (List.map colorButton model.colors)
          else
            div []
                (if model.disco then
                    [ sliderInput 10 200 5 model.discoTickDuration ChangeTickDuration
                    , (Html.text <| toString <| model.discoTickDuration)
                    ]
                 else
                    []
                )
        , div []
            [ input
                [ type' "button"
                , value "restart"
                , Html.onClick Restart
                ]
                []
            ]
        ]


sliderInput : Int -> Int -> Int -> Float -> (Float -> Msg) -> Html Msg
sliderInput minValue maxValue stepSize val msg =
    input
        [ type' "range"
        , Html.min <| toString minValue
        , Html.max <| toString maxValue
        , Html.step <| toString stepSize
        , Html.value <| toString val
        , Html.on "input" (Json.map msg Html.targetValueFloat)
        ]
        []


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
