module Main exposing (main)

import Font exposing (Font)
import Font.VT220
import Playground as P
import Set exposing (Set)


usedFont : Font
usedFont =
    Font.VT220.font


view : P.Computer -> () -> List P.Shape
view computer () =
    let
        pressedKeys : List Char
        pressedKeys =
            computer.keyboard.keys
                |> Set.toList
                |> List.filterMap toChar
    in
    List.concat
        [ [ P.rectangle P.black 600 400 ]
        , pressedKeys
            |> List.map (Font.view P.white usedFont)
            |> List.indexedMap (\i shapes -> List.map (P.moveX (toFloat (i * usedFont.charWidth))) shapes)
            |> List.concat
        ]


toChar : String -> Maybe Char
toChar key =
    case key of
        "KeyA" ->
            Just 'A'

        "KeyB" ->
            Just 'B'

        _ ->
            let
                _ =
                    Debug.log "unknown key" key
            in
            Nothing


main : Program () (P.Playground ()) P.Msg
main =
    P.game view (\_ () -> ()) ()
