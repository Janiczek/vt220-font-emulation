module Font.VT220 exposing (font)

import Dict exposing (Dict)
import Font exposing (CharData, CharMap, Font)
import Set exposing (Set)


font : Font
font =
    { charWidth = 8
    , charHeight = 10
    , charMap = map
    }


map : CharMap
map =
    Dict.fromList
        [ ( 'A', char_A )
        , ( 'B', char_B )
        ]


char_A : CharData
char_A =
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


char_B : CharData
char_B =
    Font.fromPbm
        """
00000000
11111100
01000010
01000010
01111100
01000010
01000010
11111100
00000000
00000000
        """
