port module Stylesheets exposing (..)

import Css.File exposing (..)
import Html.App as Html
import Html exposing (div)
import Style


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "style.css", compile [ Style.css ] ) ]


main : Program Never
main =
    Html.program
        { init = ( (), files cssFiles )
        , update = always always ( (), Cmd.none )
        , view = always <| div [] []
        , subscriptions = always Sub.none
        }
