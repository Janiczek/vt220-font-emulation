module Main exposing (main)

import Font exposing (Font)
import Font.VT220
import Html exposing (Html)
import Html.Attributes
import Layout


chars : List Char
chars =
    "ABCD Abcd! 0123"
        |> String.toList


main : Html msg
main =
    Layout.column { gap = 4 }
        [ section Font.VT220.font0Raw "0: original (as in ROM)"
        , section Font.VT220.font1Extended "1: right-extended"
        , section Font.VT220.font2DotStretched "2: dot-stretched"
        , section (Font.VT220.font0Raw |> Font.withScanlines) "0S: original with scanlines"
        , section (Font.VT220.font1Extended |> Font.withScanlines) "1S: right-extended with scanlines"
        , section (Font.VT220.font2DotStretched |> Font.withScanlines) "2S: dot-stretched with scanlines"
        ]


section : Font -> String -> Html msg
section font label =
    Layout.column { gap = 0 }
        [ Html.text label
        , viewWith 1 font
        , viewWith 2 font
        ]


viewWith : Int -> Font -> Html msg
viewWith scale usedFont =
    chars
        |> List.map (Font.view "white" scale usedFont)
        |> Layout.row { gap = 0 }
