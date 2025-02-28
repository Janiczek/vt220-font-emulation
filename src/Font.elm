module Font exposing
    ( Font, fromPbm
    , CharData
    , CharMap
    , view
    , PostprocessFn, phosphorLatency, crtBloom
    )

{-|

@docs Font, fromPbm
@docs CharData
@docs CharMap
@docs view
@docs PostprocessFn, phosphorLatency, crtBloom

-}

import Config exposing (Config)
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import List.Cartesian
import List.Extra


{-| Each character value is a set of "lit" (x,y) coordinates
-}
type alias CharData =
    List ( Int, Int )


type alias CharMap =
    Dict Char CharData


type alias Font =
    { charWidth : Int
    , charHeight : Int
    , charMap : CharMap
    }


{-| Expects the 0/1 part of the PBM file (not the P1 / width / height header).

For example instead of the whole file:

    P1
    8 10
    00000000
    00010000
    00101000
    01000100
    10000010
    11111110
    10000010
    10000010
    00000000
    00000000

use only the 0/1 part::

    Font.fromPbm
        """
    00000000
    00010000
    00101000
    01000100
    10000010
    11111110
    10000010
    10000010
    00000000
    00000000
        """

Any non-\\n whitespace will be trimmed.

To get a PBM file from a PNG:

    cat A.png | pngtopam | ppmtopgm | pgmtopbm -plain -threshold >A.pbm

These utilities can be found in the `netpbm` package.

-}
fromPbm : String -> CharData
fromPbm pbm =
    pbm
        |> String.trim
        |> String.lines
        |> List.indexedMap
            (\y line ->
                line
                    |> String.trim
                    |> String.toList
                    |> List.indexedMap
                        (\x c ->
                            ( ( x, y )
                            , case c of
                                '1' ->
                                    True

                                '0' ->
                                    False

                                _ ->
                                    Debug.todo "Invalid character"
                            )
                        )
            )
        |> List.concat
        |> List.filterMap
            (\( xy, isOn ) ->
                if isOn then
                    Just xy

                else
                    Nothing
            )


view : Int -> String -> PostprocessFn -> Font -> Char -> Html msg
view scale color postprocess font c =
    let
        data =
            case Dict.get c font.charMap of
                Nothing ->
                    let
                        _ =
                            Debug.log "char not found in font's char map" c
                    in
                    []

                Just charData ->
                    charData
    in
    pixelGrid
        scale
        color
        ( font.charWidth
        , font.charHeight
        )
        (data
            |> List.map (\( x, y ) -> ( x, y, 1 ))
            |> postprocess
        )


pixelGrid : Int -> String -> ( Int, Int ) -> List ( Int, Int, Float ) -> Html msg
pixelGrid scale color ( width, height ) pixels =
    Html.div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "width" (String.fromInt (width * scale) ++ "px")
        , Html.Attributes.style "height" (String.fromInt (height * scale) ++ "px")
        , Html.Attributes.style "overflow" "visible"
        ]
        (List.map (pixelDot scale color) pixels)


pixelDot : Int -> String -> ( Int, Int, Float ) -> Html msg
pixelDot scale color ( x, y, opacity ) =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "width" (String.fromInt scale ++ "px")
        , Html.Attributes.style "height" (String.fromInt scale ++ "px")
        , Html.Attributes.style "background-color" color
        , Html.Attributes.style "left" (String.fromInt (x * scale) ++ "px")
        , Html.Attributes.style "top" (String.fromInt (y * scale) ++ "px")
        , Html.Attributes.style "opacity" (String.fromFloat opacity)
        ]
        []


type alias PostprocessFn =
    List ( Int, Int, Float ) -> List ( Int, Int, Float )


{-| Assuming the opacity of the input pixels is always 1
-}
idealized : PostprocessFn
idealized =
    identity


{-| Assuming the opacity of the input pixels is always 1.

This adds horizontal "smearing":

  - the first pixel of a consecutive run is a bit
  - all other pixels of a consecutive run are full brightness
  - the pixel right after the last pixel of a consecutive run is still a bit bright

-}
phosphorLatency : Config -> PostprocessFn
phosphorLatency config =
    let
        getY ( _, y, _ ) =
            y

        getX ( x, _, _ ) =
            x
    in
    \pixels ->
        pixels
            |> List.Extra.gatherEqualsBy getY
            |> List.concatMap
                (\( fstInRow, restInRow ) ->
                    let
                        y =
                            getY fstInRow

                        allColsInRow : List Int
                        allColsInRow =
                            (fstInRow :: restInRow)
                                |> List.map getX
                                |> List.sort

                        {-
                           [1,2,3] -> [[1,2,3]]
                           [1,2,4,5,6] -> [[1,2],[4,5,6]]
                           [1,5,6,10,11] -> [[1],[5,6],[10,11]]
                        -}
                        consecutiveRuns : List (List Int)
                        consecutiveRuns =
                            allColsInRow
                                |> List.Extra.groupWhile (\a b -> b == a + 1)
                                |> List.map List.Extra.cons

                        point : Float -> Int -> ( Int, Int, Float )
                        point opacity x =
                            ( x, y, opacity )
                    in
                    consecutiveRuns
                        |> List.map toNonemptyList
                        |> List.concatMap
                            (\( first, rest ) ->
                                let
                                    last : Int
                                    last =
                                        List.Extra.last rest
                                            |> Maybe.withDefault first
                                in
                                List.concat
                                    [ [ point config.phosphorLatencyFirst first ]
                                    , List.map (point config.phosphorLatencyFull) rest
                                    , [ point config.phosphorLatencyAfterLast (last + 1) ]
                                    ]
                            )
                )
            |> sumCollidingXYs config


toNonemptyList : List a -> ( a, List a )
toNonemptyList xs =
    case xs of
        [] ->
            Debug.todo "unexpected empty list"

        x :: xs_ ->
            ( x, xs_ )


{-| Bloom: each lit pixel will add x% of its light (is it linear?) to
its neighbours.
-}
crtBloom : Config -> PostprocessFn
crtBloom config =
    \pixels ->
        let
            bloomDiagonal =
                config.bloomDiagonalStrength

            bloomOrthogonal =
                config.bloomOrthogonalStrength

            self =
                ( 0, 0, 1 )

            diagonal =
                [ ( -1, -1, bloomDiagonal )
                , ( -1, 1, bloomDiagonal )
                , ( 1, -1, bloomDiagonal )
                , ( 1, 1, bloomDiagonal )
                ]

            orthogonal =
                vertical
                    ++ [ ( -1, 0, bloomOrthogonal )
                       , ( 1, 0, bloomOrthogonal )
                       ]

            vertical =
                [ down
                , ( 0, -1, bloomOrthogonal )
                ]

            down =
                ( 0, 1, bloomOrthogonal )

            bloom : ( Int, Int, Float ) -> List ( Int, Int, Float )
            bloom ( x, y, opacity ) =
                (case config.bloom of
                    Config.BloomEverywhere ->
                        self :: List.concat [ diagonal, orthogonal ]

                    Config.BloomOrthogonal ->
                        self :: orthogonal

                    Config.BloomVertical ->
                        self :: vertical

                    Config.BloomDown ->
                        [ self, down ]
                )
                    |> List.map (\( dx, dy, k ) -> ( x + dx, y + dy, opacity * k ))
        in
        pixels
            |> List.concatMap bloom
            |> sumCollidingXYs config


{-| We're merging cells occupying the same space, so that later postprocessing
doesn't make things >100%.

For that reason we're also clamping them to 1.

-}
sumCollidingXYs : Config -> List ( Int, Int, Float ) -> List ( Int, Int, Float )
sumCollidingXYs config cells =
    if config.clampCellBrightness then
        cells
            |> List.foldl
                (\( x_, y_, opacity ) ->
                    Dict.update ( x_, y_ )
                        (\maybeOpacity ->
                            case maybeOpacity of
                                Nothing ->
                                    Just opacity

                                Just currentOpacity ->
                                    Just (min 1 (currentOpacity + opacity))
                        )
                )
                Dict.empty
            |> Dict.toList
            |> List.map (\( ( x_, y_ ), opacity ) -> ( x_, y_, opacity ))

    else
        cells
