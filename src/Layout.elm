module Layout exposing (column, grid, row)

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


grid : { gapX : Int, gapY : Int } -> List (List (Html msg)) -> Html msg
grid { gapX, gapY } items =
    let
        columns =
            items
                |> List.head
                |> Maybe.withDefault []
                |> List.length
                |> Debug.log "columns"
    in
    Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" ("repeat(" ++ String.fromInt columns ++ ", fit-content(100%))")
        , Html.Attributes.style "row-gap" (String.fromInt gapY ++ "px")
        , Html.Attributes.style "column-gap" (String.fromInt gapX ++ "px")
        , Html.Attributes.style "align-items" "center"
        ]
        (List.concat items)
