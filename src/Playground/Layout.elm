module Playground.Layout exposing (column, row)

import Playground as P


column : List { height : Int, shape : P.Shape } -> P.Shape
column shapes =
    shapes
        |> List.foldr
            (\shape ( accY, accItems ) ->
                ( accY + toFloat shape.height
                , P.moveY (accY + toFloat shape.height) shape.shape :: accItems
                )
            )
            ( 0, [] )
        |> Tuple.second
        |> P.group


row : List { width : Int, shape : P.Shape } -> P.Shape
row shapes =
    shapes
        |> List.foldl
            (\shape ( accX, accItems ) ->
                ( accX + toFloat shape.width
                , P.moveX (accX + toFloat shape.width) shape.shape :: accItems
                )
            )
            ( 0, [] )
        |> Tuple.second
        |> P.group
