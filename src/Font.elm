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


view : String -> PostprocessFn -> Font -> Char -> Html msg
view color postprocess font c =
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
        ( font.charWidth
        , font.charHeight
        )
        (data
            |> List.map (\( x, y ) -> ( x, y, 1 ))
            |> postprocess
        )


pixelGrid : ( Int, Int ) -> List ( Int, Int, Float ) -> Html msg
pixelGrid ( width, height ) pixels =
    Html.div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "height" (String.fromInt height ++ "px")
        , Html.Attributes.style "overflow" "visible"
        ]
        (List.map pixelDot pixels)


pixelDot : ( Int, Int, Float ) -> Html msg
pixelDot ( x, y, opacity ) =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "width" "1px"
        , Html.Attributes.style "height" "1px"
        , Html.Attributes.style "background-color" "white"
        , Html.Attributes.style "left" (String.fromInt x ++ "px")
        , Html.Attributes.style "top" (String.fromInt y ++ "px")
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
phosphorLatency : PostprocessFn
phosphorLatency =
    let
        getY ( _, y, _ ) =
            y

        getX ( x, _, _ ) =
            x

        firstOpacity : Float
        firstOpacity =
            169 / 255

        fullOpacity : Float
        fullOpacity =
            1

        afterLastOpacity : Float
        afterLastOpacity =
            82 / 255
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
                            let
                                go : List Int -> Int -> List Int -> List (List Int) -> List (List Int)
                                go currentRun currentRight todos acc =
                                    case todos of
                                        [] ->
                                            List.reverse (currentRight :: currentRun) :: acc

                                        next :: restOfTodos ->
                                            if next == currentRight + 1 then
                                                go (currentRight :: currentRun) next restOfTodos acc

                                            else
                                                go [] next restOfTodos (List.reverse (currentRight :: currentRun) :: acc)
                            in
                            case allColsInRow of
                                [] ->
                                    []

                                col :: cols ->
                                    go [] col cols []

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
                                    [ [ point firstOpacity first ]
                                    , List.map (point fullOpacity) rest
                                    , [ point afterLastOpacity (last + 1) ]
                                    ]
                            )
                        -- Make sure we clamp to 1 on any given pixel
                        |> List.foldl
                            (\( x_, y_, opacity ) ->
                                Dict.update ( x_, y_ )
                                    (\maybeOpacity ->
                                        case maybeOpacity of
                                            Nothing ->
                                                Just opacity

                                            Just currentOpacity ->
                                                Just (max 1 (currentOpacity + opacity))
                                    )
                            )
                            Dict.empty
                        |> Dict.toList
                        |> List.map (\( ( x_, y_ ), opacity ) -> ( x_, y_, opacity ))
                )
            |> Debug.log "crt latency"


toNonemptyList : List a -> ( a, List a )
toNonemptyList xs =
    case xs of
        [] ->
            Debug.todo "unexpected empty list"

        x :: xs_ ->
            ( x, xs_ )


{-| Blom: each lit pixel will add x% of its light (is it linear?) to
the pixel below it.

TODO: should it also bleed above it? To the sides? All around?

based on VTterm-vt100-charwidths.png:

    F  S+ A           F  = first pixel (warming up)
    86 0 173          S+ = second+ pixel (full brightness)
    231 218 242       A  = after pixel (cooling down)

    a) where numbers go 0..243
    b) where numbers go 0..255

    157     243    70
    12      25     1
    ---------------------
    0.0764  0.1029 0.0124

    169     255    82
    24      37     13
    ---------------------
    0.1420  0.1451 0.1585

    I'm making it be 0.1451 everywhere...

-}
crtBloom : PostprocessFn
crtBloom =
    \pixels ->
        let
            bleedBelow =
                37 / 255

            bloom : ( Int, Int, Float ) -> List ( Int, Int, Float )
            bloom ( x, y, opacity ) =
                [ ( x, y, opacity )
                , ( x, y + 1, opacity * bleedBelow )
                ]
        in
        pixels
            |> List.concatMap bloom
