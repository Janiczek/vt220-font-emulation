module Font exposing (CharData, CharMap, Font, fromPbm, view)

import Dict exposing (Dict)
import Playground as P


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

use this:

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


view : P.Color -> Font -> Char -> P.Shape
view color font c =
    case Dict.get c font.charMap of
        Nothing ->
            --let
            --    _ =
            --        Debug.log "char not in font's char map" c
            --in
            P.group []

        Just charData ->
            charData
                |> List.map
                    (\( x, y ) ->
                        P.rectangle color 1 1
                            |> P.move (toFloat x) (toFloat -y)
                    )
                |> P.group
