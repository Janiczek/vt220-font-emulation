module Font.VT220 exposing (font0Raw, font1Extended, font2DotStretched)

import Dict exposing (Dict)
import Font exposing (CharData, CharMap, Font)
import Font.VT220.Chars exposing (chars)


font0Raw : Font
font0Raw =
    { charWidth = 8
    , charHeight = 10
    , charMap = map
    }


font1Extended : Font
font1Extended =
    { font0Raw
        | charWidth = font0Raw.charWidth + 2
        , charMap =
            font0Raw.charMap
                |> Dict.map (\_ -> extendRightSide (font0Raw.charWidth - 1))
    }


font2DotStretched : Font
font2DotStretched =
    { font1Extended
        | charWidth = font1Extended.charWidth + 1
        , charMap =
            font1Extended.charMap
                |> Dict.map (\_ -> dotStretch)
    }


map : CharMap
map =
    Dict.fromList
        [ ( 'A', chars.aUpper )
        , ( 'B', chars.bUpper )
        , ( 'C', chars.cUpper )
        , ( 'D', chars.dUpper )

        --
        , ( 'b', chars.bLower )
        , ( 'c', chars.cLower )
        , ( 'd', chars.dLower )

        --
        , ( '!', chars.bang )

        --
        , ( '0', chars.digit0 )
        , ( '1', chars.digit1 )
        , ( '2', chars.digit2 )
        , ( '3', chars.digit3 )
        ]


extendRightSide : Int -> CharData -> CharData
extendRightSide lastColumn charData =
    charData
        |> List.concatMap
            (\( x, y ) ->
                if x == lastColumn then
                    [ ( x, y )
                    , ( x + 1, y )
                    , ( x + 2, y )
                    ]

                else
                    [ ( x, y ) ]
            )


dotStretch : CharData -> CharData
dotStretch charData =
    charData
        |> List.concatMap
            (\( x, y ) ->
                [ ( x, y )
                , ( x + 1, y )
                ]
            )
