module Layout exposing (column, pixelGrid, row)

import Html exposing (Html)
import Html.Attributes


column : { gap : Int } -> List (Html msg) -> Html msg
column { gap } items =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" (String.fromInt gap ++ "px")
        ]
        items


row : { gap : Int } -> List (Html msg) -> Html msg
row { gap } items =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "row"
        , Html.Attributes.style "gap" (String.fromInt gap ++ "px")
        ]
        items


pixelGrid : ( Int, Int ) -> List ( Int, Int ) -> Html msg
pixelGrid ( width, height ) pixels =
    Html.div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "height" (String.fromInt height ++ "px")
        ]
        (List.map pixelDot pixels)


pixelDot : ( Int, Int ) -> Html msg
pixelDot ( x, y ) =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "width" "1px"
        , Html.Attributes.style "height" "1px"
        , Html.Attributes.style "background-color" "white"
        , Html.Attributes.style "left" (String.fromInt x ++ "px")
        , Html.Attributes.style "top" (String.fromInt y ++ "px")
        ]
        []
