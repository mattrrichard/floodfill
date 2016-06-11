module Main exposing (..)

import Html.App as Html
import Html exposing (..)


type alias Model =
    {
    }


type Msg
    = NoOp


main : Program Never
main =
    Html.beginnerProgram
        { model = {}
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div [] []
