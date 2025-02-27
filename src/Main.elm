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
        [ section Font.VT220.font.singleWidthRaw "original (as in ROM)"
        , section Font.VT220.font.singleWidthExtended "right-extended"
        , section Font.VT220.font.singleWidthDotStretched "dot-stretched"
        , section (Font.VT220.font.singleWidthRaw |> Font.withScanlines) "original with scanlines"
        , section (Font.VT220.font.singleWidthExtended |> Font.withScanlines) "right-extended with scanlines"
        , section (Font.VT220.font.singleWidthDotStretched |> Font.withScanlines) "dot-stretched with scanlines"
        , section Font.VT220.font.doubleWidthRaw "double-width"
        , section Font.VT220.font.doubleWidthExtended "double-width right-extended"
        , section Font.VT220.font.doubleWidthDotStretched "double-width dot-stretched"
        , section (Font.VT220.font.doubleWidthRaw |> Font.withScanlines) "double-width with scanlines"
        , section (Font.VT220.font.doubleWidthExtended |> Font.withScanlines) "double-width right-extended with scanlines"
        , section (Font.VT220.font.doubleWidthDotStretched |> Font.withScanlines) "double-width dot-stretched with scanlines"
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
