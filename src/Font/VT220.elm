module Font.VT220 exposing (font)

import Dict exposing (Dict)
import Font exposing (CharData, CharMap, Font)
import Font.VT220.Chars as Chars


map : CharMap
map =
    Chars.map


raw : Font
raw =
    { charWidth = 8
    , charHeight = 10
    , charMap = map
    }


font :
    { singleWidthRaw : Font
    , doubleWidthRaw : Font
    , singleWidthExtended : Font
    , doubleWidthExtended : Font
    , singleWidthDotStretched : Font
    , doubleWidthDotStretched : Font
    , singleWidthScanlines : Font
    , doubleWidthScanlines : Font
    }
font =
    { singleWidthRaw = raw
    , doubleWidthRaw = raw |> doubleWidth
    , singleWidthExtended = raw |> extendRightSide
    , doubleWidthExtended = raw |> doubleWidth |> extendRightSide
    , singleWidthDotStretched = raw |> extendRightSide |> dotStretch
    , doubleWidthDotStretched = raw |> doubleWidth |> extendRightSide |> dotStretch
    , singleWidthScanlines = raw |> extendRightSide |> dotStretch |> verticallyDouble
    , doubleWidthScanlines = raw |> doubleWidth |> extendRightSide |> dotStretch |> verticallyDouble
    }


doubleWidth : Font -> Font
doubleWidth f =
    { f
        | charWidth = f.charWidth * 2
        , charMap = f.charMap |> Dict.map (\_ -> doubleWidthCharData)
    }


doubleWidthCharData : CharData -> CharData
doubleWidthCharData charData =
    charData
        |> List.concatMap
            (\( x, y ) ->
                -- There will be no overlap in this transformation
                [ ( x * 2, y )
                , ( x * 2 + 1, y )
                ]
            )


extendRightSide : Font -> Font
extendRightSide f =
    { f
        | charWidth = f.charWidth + 2
        , charMap =
            f.charMap
                |> Dict.map (\_ -> extendRightSideCharData (f.charWidth - 1))
    }


extendRightSideCharData : Int -> CharData -> CharData
extendRightSideCharData lastColumn charData =
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


dotStretch : Font -> Font
dotStretch f =
    { f
        | charWidth = f.charWidth + 1
        , charMap =
            f.charMap |> Dict.map (\_ -> dotStretchCharData)
    }


dotStretchCharData : CharData -> CharData
dotStretchCharData charData =
    charData
        |> List.concatMap
            (\( x, y ) ->
                -- There will be overlap between neighbouring pixels - this is intentional
                [ ( x, y )
                , ( x + 1, y )
                ]
            )


verticallyDouble : Font -> Font
verticallyDouble f =
    { f
        | charHeight = f.charHeight * 2
        , charMap =
            f.charMap
                |> Dict.map (\_ -> List.map (\( x, y ) -> ( x, y * 2 )))
    }
