module Main exposing (main)

import Font exposing (Font)
import Font.VT220
import Playground as P
import Playground.Layout as PL
import Set exposing (Set)


textToShow : List Char
textToShow =
    "ABCD Abcd! 0123"
        |> String.toList


view : P.Computer -> () -> List P.Shape
view computer () =
    let
        pressedKeys : List Char
        pressedKeys =
            computer.keyboard.keys
                |> Set.toList
                |> List.filterMap toChar

        text : List Char
        text =
            -- pressedKeys
            textToShow

        viewWith : Int -> Font -> P.Shape
        viewWith scale usedFont =
            text
                |> List.map
                    (\c ->
                        { width = usedFont.charWidth * scale
                        , shape =
                            Font.view P.white usedFont c
                                |> P.scale (toFloat scale)
                        }
                    )
                |> PL.row

        font : Font -> Int -> { height : Int, shape : P.Shape }
        font usedFont scale =
            { height = usedFont.charHeight
            , shape = viewWith scale usedFont
            }
    in
    [ P.rectangle P.black 600 400
    , PL.column
        [ font Font.VT220.font0Raw 1
        , font Font.VT220.font0Raw 2
        , font Font.VT220.font1Extended 1
        , font Font.VT220.font1Extended 2
        , font Font.VT220.font2DotStretched 1
        , font Font.VT220.font2DotStretched 2
        ]
    ]


toChar : String -> Maybe Char
toChar key =
    case key of
        "KeyA" ->
            Just 'A'

        "KeyB" ->
            Just 'B'

        "KeyC" ->
            Just 'C'

        "KeyD" ->
            Just 'D'

        _ ->
            --let
            --    _ =
            --        Debug.log "unknown key" key
            --in
            Nothing


main : Program () (P.Playground ()) P.Msg
main =
    P.game view (\_ () -> ()) ()
