module Main exposing (main)

import Font exposing (Font)
import Font.VT220
import Html exposing (Html)
import Html.Attributes
import Layout



{- TODO:

   A) horizontal smearing

   first pixel of a line will be slightly lighter
   pixel after the last pixel of a line will be slightly lit

   B) vertical smearing

   each lit pixel will add x% of its light (is it linear?) to the pixels above and below it

   ---------------

   based on VTterm-vt100-charwidths.png:

   F  S+ A           F  = first pixel (warming up)
   86 0 173          S+ = second+ pixel (full brightness)
   231 218 243       A  = after pixel (cooling down)

   a) where numbers go 0..243
   b) where numbers go 0..255
   c) where numbers go 0..255 (but change the bottom right number to 255)
-}


chars : List Char
chars =
    "ABCD Abcd! 0123"
        |> String.toList


main : Html msg
main =
    Layout.grid { gap = 8 }
        [ section Font.VT220.font.singleWidthRaw "0: original (as in ROM)"
        , section Font.VT220.font.singleWidthExtended "1: right-extended"
        , section Font.VT220.font.singleWidthDotStretched "2: dot-stretched"
        , section (Font.VT220.font.singleWidthRaw |> Font.withScanlines) "0S: original with scanlines"
        , section (Font.VT220.font.singleWidthExtended |> Font.withScanlines) "1S: right-extended with scanlines"
        , section (Font.VT220.font.singleWidthDotStretched |> Font.withScanlines) "2S: dot-stretched with scanlines"
        , section Font.VT220.font.doubleWidthRaw "0D: original (as in ROM)"
        , section Font.VT220.font.doubleWidthExtended "1D: right-extended"
        , section Font.VT220.font.doubleWidthDotStretched "2D: dot-stretched"
        , section (Font.VT220.font.doubleWidthRaw |> Font.withScanlines) "0DS: original with scanlines"
        , section (Font.VT220.font.doubleWidthExtended |> Font.withScanlines) "1DS: right-extended with scanlines"
        , section (Font.VT220.font.doubleWidthDotStretched |> Font.withScanlines) "2DS: dot-stretched with scanlines"
        ]


section : Font -> String -> List (Html msg)
section font label =
    [ Html.span [] [ Html.text label ]
    , Layout.column { gap = 0 }
        [ viewWith 1 font
        , viewWith 2 font
        ]
    ]


viewWith : Int -> Font -> Html msg
viewWith scale usedFont =
    chars
        |> List.map (Font.view "white" scale usedFont)
        |> Layout.row { gap = 0 }
