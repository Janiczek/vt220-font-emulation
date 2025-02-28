module Main exposing (main)

import Font exposing (Font, PostprocessFn)
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
    Layout.grid { gapX = 16, gapY = 4 }
        -- single-width
        [ section identity Font.VT220.font.singleWidthRaw "original (as in ROM)"
        , section identity Font.VT220.font.singleWidthExtended "+ right-extension"
        , section identity Font.VT220.font.singleWidthDotStretched "+ dot stretching"
        , section identity Font.VT220.font.singleWidthScanlines "+ scanlines"
        , section Font.phosphorLatency Font.VT220.font.singleWidthScanlines "+ phosphor latency"
        , section (Font.phosphorLatency >> Font.crtBloom) Font.VT220.font.singleWidthScanlines "+ CRT bloom"

        -- double-width
        , section identity Font.VT220.font.doubleWidthRaw "original + double-width"
        , section identity Font.VT220.font.doubleWidthExtended "+ right-extension"
        , section identity Font.VT220.font.doubleWidthDotStretched "+ dot stretching"
        , section identity Font.VT220.font.doubleWidthScanlines "+ scanlines"
        , section Font.phosphorLatency Font.VT220.font.doubleWidthScanlines "+ phosphor latency"
        , section (Font.phosphorLatency >> Font.crtBloom) Font.VT220.font.doubleWidthScanlines "+ CRT bloom"
        ]


section : PostprocessFn -> Font -> String -> List (Html msg)
section postprocess font label =
    [ Html.span [] [ Html.text label ]
    , viewWith postprocess font
    ]


viewWith : PostprocessFn -> Font -> Html msg
viewWith postprocess usedFont =
    chars
        |> List.map (Font.view "white" postprocess usedFont)
        |> Layout.row { gap = 0 }
