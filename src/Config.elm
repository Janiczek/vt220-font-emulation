module Config exposing (Bloom(..), Config, initKnob)

import Knob exposing (Knob)


type alias Config =
    { scale : Int
    , text : String

    --
    , phosphorLatencyFirst : Float
    , phosphorLatencyFull : Float
    , phosphorLatencyAfterLast : Float

    --
    , bloom : Bloom
    , bloomDiagonalStrength : Float
    , bloomOrthogonalStrength : Float

    --
    , clampCellBrightness : Bool

    --
    , amberColor : Knob.Color
    , blueColor : Knob.Color
    , greenColor : Knob.Color
    }


type Bloom
    = BloomEverywhere
    | BloomOrthogonal
    | BloomVertical
    | BloomDown


bloomFromString : String -> Bloom
bloomFromString s =
    case s of
        "Everywhere" ->
            BloomEverywhere

        "Orthogonal" ->
            BloomOrthogonal

        "Vertical" ->
            BloomVertical

        "Down" ->
            BloomDown

        _ ->
            Debug.todo "unknown bloom"


bloomToString : Bloom -> String
bloomToString b =
    case b of
        BloomEverywhere ->
            "Everywhere"

        BloomOrthogonal ->
            "Orthogonal"

        BloomVertical ->
            "Vertical"

        BloomDown ->
            "Down"


init : Config
init =
    { scale = 2
    , text = "ABCD Abcd! 0123"

    --
    , phosphorLatencyFirst = 169 / 255
    , phosphorLatencyFull = 1
    , phosphorLatencyAfterLast = 82 / 255

    --
    , bloom = BloomEverywhere
    , bloomDiagonalStrength = 6 / 255
    , bloomOrthogonalStrength = 37 / 255

    --
    , clampCellBrightness = True

    --
    , amberColor = { red = 255 / 255, green = 183 / 255, blue = 0 / 255 }
    , blueColor = { red = 153 / 255, green = 221 / 255, blue = 255 / 255 }
    , greenColor = { red = 51 / 255, green = 255 / 255, blue = 51 / 255 }
    }


initKnob : Knob Config
initKnob =
    Knob.compose Config
        |> Knob.stackLabel "Scale" (Knob.intSlider { range = ( 1, 8 ), step = 1, initial = init.scale })
        |> Knob.stackLabel "Text" (Knob.stringInput init.text)
        |> Knob.stackLabel "Phosphor latency - first pixel" (Knob.floatSlider { range = ( 0, 1 ), step = 0.01, initial = init.phosphorLatencyFirst })
        |> Knob.stackLabel "Phosphor latency - second+ pixels" (Knob.floatSlider { range = ( 0, 1 ), step = 0.01, initial = init.phosphorLatencyFull })
        |> Knob.stackLabel "Phosphor latency - pixel after the last" (Knob.floatSlider { range = ( 0, 1 ), step = 0.01, initial = init.phosphorLatencyAfterLast })
        |> Knob.stackLabel "Bloom"
            (Knob.select
                { options = [ "Everywhere", "Orthogonal", "Vertical", "Down" ]
                , fromString = bloomFromString
                , toString = bloomToString
                , initial = init.bloom
                }
            )
        |> Knob.stackLabel "Bloom diagonal strength" (Knob.floatSlider { range = ( 0, 1 ), step = 0.01, initial = init.bloomDiagonalStrength })
        |> Knob.stackLabel "Bloom orthogonal strength" (Knob.floatSlider { range = ( 0, 1 ), step = 0.01, initial = init.bloomOrthogonalStrength })
        |> Knob.stackLabel "Clamp cell brightness" (Knob.boolCheckbox init.clampCellBrightness)
        |> Knob.stackLabel "Amber color" (Knob.colorPicker init.amberColor)
        |> Knob.stackLabel "Blue color" (Knob.colorPicker init.blueColor)
        |> Knob.stackLabel "Green color" (Knob.colorPicker init.greenColor)
