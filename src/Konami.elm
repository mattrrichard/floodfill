module Konami exposing (State, init, update)


type Key
    = Left
    | Up
    | Right
    | Down
    | A
    | B
    | Unknown


type State
    = State (List Key)


init : State
init =
    State []


goalState : List Key
goalState =
    List.reverse [ Up, Up, Down, Down, Left, Right, Left, Right, B, A ]


fromCode : Int -> Key
fromCode code =
    case code of
        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        65 ->
            A

        66 ->
            B

        _ ->
            Unknown


update : Int -> State -> ( State, Bool )
update keyCode (State state) =
    case fromCode keyCode of
        Unknown ->
            ( State [], False )

        key ->
            let
                newState =
                    List.take 10 (key :: state)
            in
                ( State newState, newState == goalState )
