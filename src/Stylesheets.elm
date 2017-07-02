port module Stylesheets exposing (..)

import Css.File exposing (..)
import Style


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "style.css", compile [ Style.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files cssFiles
