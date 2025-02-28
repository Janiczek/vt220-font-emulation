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


{-| Amber phosphor
#d3a40a: P3 phosphor
#ffb700
-}
amber : String
amber =
    "#ffb700"


{-| VT220 blue
#99ddff
-}
blue : String
blue =
    "#99ddff"


{-| Green phosphor
#33ff33: P1 phosphor
#66ff66: P24 phosphor
#4aff00
#33ff66
-}
green : String
green =
    "#33ff33"


main : Html msg
main =
    let
        i =
            identity

        h =
            Font.phosphorLatency

        hv =
            Font.phosphorLatency >> Font.crtBloom
    in
    Layout.grid { gapX = 16, gapY = 4 }
        -- single-width
        [ section "white" i Font.VT220.font.singleWidthRaw "original (as in ROM)"
        , section "white" i Font.VT220.font.singleWidthExtended "+ right-extension"
        , section "white" i Font.VT220.font.singleWidthDotStretched "+ dot stretching"
        , section "white" i Font.VT220.font.singleWidthScanlines "+ scanlines"
        , section "white" h Font.VT220.font.singleWidthScanlines "+ phosphor latency"
        , section "white" hv Font.VT220.font.singleWidthScanlines "+ CRT bloom"
        , section amber hv Font.VT220.font.singleWidthScanlines "@ amber"
        , section blue hv Font.VT220.font.singleWidthScanlines "@ blue"
        , section green hv Font.VT220.font.singleWidthScanlines "@ green"

        --
        , divider

        -- double-width
        , section "white" i Font.VT220.font.doubleWidthRaw "original + double-width"
        , section "white" i Font.VT220.font.doubleWidthExtended "+ right-extension"
        , section "white" i Font.VT220.font.doubleWidthDotStretched "+ dot stretching"
        , section "white" i Font.VT220.font.doubleWidthScanlines "+ scanlines"
        , section "white" h Font.VT220.font.doubleWidthScanlines "+ phosphor latency"
        , section "white" hv Font.VT220.font.doubleWidthScanlines "+ CRT bloom"
        , section amber hv Font.VT220.font.doubleWidthScanlines "@ amber"
        , section blue hv Font.VT220.font.doubleWidthScanlines "@ blue"
        , section green hv Font.VT220.font.doubleWidthScanlines "@ green"
        ]


divider : List (Html msg)
divider =
    [ Html.div [ Html.Attributes.style "height" "1em" ] []
    , Html.div [] []
    ]


section : String -> PostprocessFn -> Font -> String -> List (Html msg)
section color postprocess font label =
    [ Html.span [] [ Html.text label ]
    , viewWith color postprocess font
    ]


viewWith : String -> PostprocessFn -> Font -> Html msg
viewWith color postprocess usedFont =
    chars
        |> List.map (Font.view color postprocess usedFont)
        |> Layout.row { gap = 0 }
