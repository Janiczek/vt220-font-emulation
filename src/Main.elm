module Main exposing (main)

import Browser
import Color
import Config exposing (Config)
import Font exposing (Font, PostprocessFn)
import Font.VT220
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Knob exposing (Knob)
import Knob.Option
import Layout


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    Knob Config


init : () -> ( Model, Cmd Msg )
init () =
    ( Config.initKnob, Cmd.none )


type Msg
    = KnobUpdated (Knob Config)
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        KnobUpdated knob ->
            ( knob, Cmd.none )

        Reset ->
            ( Config.initKnob, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "VT220 Font Emulation"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    let
        config : Config
        config =
            Knob.value model

        i : PostprocessFn
        i =
            identity

        h : PostprocessFn
        h =
            Font.phosphorLatency config

        hv : PostprocessFn
        hv =
            Font.phosphorLatency config >> Font.crtBloom config

        amber =
            config.amberColor |> toCssString

        blue =
            config.blueColor |> toCssString

        green =
            config.greenColor |> toCssString
    in
    Layout.row { gap = 96 }
        [ Layout.column { gap = 16 }
            [ Html.button
                [ Html.Events.onClick Reset
                , Html.Attributes.style "width" "fit-content"
                ]
                [ Html.text "Reset" ]
            , Knob.viewWithOptions [ Knob.Option.detached ] KnobUpdated model
            , Knob.styles
            ]
        , Layout.grid { gapX = 16, gapY = 4 }
            -- single-width
            [ section config "white" i Font.VT220.font.singleWidthRaw "original (as in ROM)"
            , section config "white" i Font.VT220.font.singleWidthExtended "+ right-extension"
            , section config "white" i Font.VT220.font.singleWidthDotStretched "+ dot stretching"
            , section config "white" i Font.VT220.font.singleWidthScanlines "+ scanlines"
            , section config "white" h Font.VT220.font.singleWidthScanlines "+ phosphor latency"
            , section config "white" hv Font.VT220.font.singleWidthScanlines "+ CRT bloom"
            , section config amber hv Font.VT220.font.singleWidthScanlines "@ amber"
            , section config blue hv Font.VT220.font.singleWidthScanlines "@ blue"
            , section config green hv Font.VT220.font.singleWidthScanlines "@ green"

            --
            , divider

            -- double-width
            , section config "white" i Font.VT220.font.doubleWidthRaw "original + double-width"
            , section config "white" i Font.VT220.font.doubleWidthExtended "+ right-extension"
            , section config "white" i Font.VT220.font.doubleWidthDotStretched "+ dot stretching"
            , section config "white" i Font.VT220.font.doubleWidthScanlines "+ scanlines"
            , section config "white" h Font.VT220.font.doubleWidthScanlines "+ phosphor latency"
            , section config "white" hv Font.VT220.font.doubleWidthScanlines "+ CRT bloom"
            , section config amber hv Font.VT220.font.doubleWidthScanlines "@ amber"
            , section config blue hv Font.VT220.font.doubleWidthScanlines "@ blue"
            , section config green hv Font.VT220.font.doubleWidthScanlines "@ green"
            ]
        ]


divider : List (Html msg)
divider =
    [ Html.div [ Html.Attributes.style "height" "1em" ] []
    , Html.div [] []
    ]


section : Config -> String -> PostprocessFn -> Font -> String -> List (Html msg)
section config color postprocess font label =
    [ Html.span [] [ Html.text label ]
    , viewWith config color postprocess font
    ]


viewWith : Config -> String -> PostprocessFn -> Font -> Html msg
viewWith config color postprocess usedFont =
    config.text
        |> String.toList
        |> List.map (Font.view config.scale color postprocess usedFont)
        |> Layout.row { gap = 0 }


toCssString : Knob.Color -> String
toCssString color =
    Color.toCssString (Color.rgb color.red color.green color.blue)
