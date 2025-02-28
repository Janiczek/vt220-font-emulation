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

   157 243 70     0.646 1.000 0.288
   12  25  0      0.049 0.103 0.000

   169 255 82     0.663 1.000 0.322
   24  37  12     0.094 0.145 0.047

   169 255 82     0.663 1.000 0.322
   24  37  0      0.094 0.145 0.000

-}


chars : List Char
chars =
    "ABCD Abcd! 0123"
        |> String.toList


main : Html msg
main =
    Layout.grid { gapX = 16, gapY = 4 }
        -- single-width
        [ section Font.idealized Font.VT220.font.singleWidthRaw "original (as in ROM)"
        , section Font.idealized Font.VT220.font.singleWidthExtended "+ right-extension"
        , section Font.idealized Font.VT220.font.singleWidthDotStretched "+ dot stretching"
        , section Font.idealized Font.VT220.font.singleWidthScanlines "+ scanlines"
        , section Font.withHorizontalSmearing Font.VT220.font.singleWidthScanlines "> CRT horiz. smearing"

        -- double-width
        , section Font.idealized Font.VT220.font.doubleWidthRaw "original + double-width"
        , section Font.idealized Font.VT220.font.doubleWidthExtended "+ right-extension"
        , section Font.idealized Font.VT220.font.doubleWidthDotStretched "+ dot stretching"
        , section Font.idealized Font.VT220.font.doubleWidthScanlines "+ scanlines"
        , section Font.withHorizontalSmearing Font.VT220.font.doubleWidthScanlines "> CRT horiz. smearing"
        ]


section : (List ( Int, Int ) -> List ( Int, Int, Float )) -> Font -> String -> List (Html msg)
section addOpacities font label =
    [ Html.span [] [ Html.text label ]
    , viewWith addOpacities font
    ]


viewWith : (List ( Int, Int ) -> List ( Int, Int, Float )) -> Font -> Html msg
viewWith addOpacities usedFont =
    chars
        |> List.map (Font.view "white" addOpacities usedFont)
        |> Layout.row { gap = 0 }
