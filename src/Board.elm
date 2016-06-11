module Board exposing (..)


type alias Config =
    { cols : Int
    , rows : Int
    , cellSize : Int
    , borderSize : Int
    , width : Int
    , height : Int
    }


init : Int -> Int -> Int -> Int -> Config
init rows cols cellSize borderSize =
    let
        width =
            cols * cellSize + (cols + 1) * borderSize

        height =
            rows * cellSize + (rows + 1) * borderSize
    in
        { cols = cols
        , rows = rows
        , cellSize = cellSize
        , borderSize = borderSize
        , width = width
        , height = height
        }
