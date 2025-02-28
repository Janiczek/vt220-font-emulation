module Font exposing
    ( Font, fromPbm
    , CharData
    , CharMap
    , view
    , AddOpacitiesFn, idealized, withHorizontalSmearing, withVerticalSmearing
    )

{-|

@docs Font, fromPbm
@docs CharData
@docs CharMap
@docs view
@docs AddOpacitiesFn, idealized, withHorizontalSmearing, withVerticalSmearing

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


view : String -> AddOpacitiesFn -> Font -> Char -> Html msg
view color addOpacities font c =
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
        (data |> addOpacities)


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


type alias AddOpacitiesFn =
    List ( Int, Int ) -> List ( Int, Int, Float )


idealized : AddOpacitiesFn
idealized =
    List.map (\( x, y ) -> ( x, y, 1 ))


withHorizontalSmearing : AddOpacitiesFn
withHorizontalSmearing =
    let
        getY ( _, y ) =
            y

        getX ( x, _ ) =
            x

        firstOpacity : Float
        firstOpacity =
            -- dimmer alternative: 157 / 243
            169 / 255

        fullOpacity : Float
        fullOpacity =
            1

        afterLastOpacity : Float
        afterLastOpacity =
            -- dimmer alternative: 70 / 243
            82 / 255
    in
    \pixels ->
        pixels
            |> List.Extra.gatherEqualsBy getY
            |> List.concatMap
                (\( fstXY, restOfXYs ) ->
                    let
                        y =
                            getY fstXY

                        allColsInRow : List Int
                        allColsInRow =
                            (fstXY :: restOfXYs)
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
                )


toNonemptyList : List a -> ( a, List a )
toNonemptyList xs =
    case xs of
        [] ->
            Debug.todo "unexpected empty list"

        x :: xs_ ->
            ( x, xs_ )


withVerticalSmearing : AddOpacitiesFn
withVerticalSmearing =
    \pixels ->
        Debug.todo "vertical smearing"
