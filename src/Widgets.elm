module Widgets exposing (blue, fromHex, padding_, red, theme, white)

import Color exposing (rgb)
import Color.Convert exposing (hexToColor)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


theme model content =
    layout [ width fill ] <|
        column [ width (fill |> maximum 800), centerX, spacing 20, Font.size 16, padding_ 0 20 0 20 ]
            [ -- row [ centerX, padding 50, width fill ]
              --     [ column [ spacing 30, width fill ]
              --         [
              --         -- image [ centerX, width (fill |> maximum 300) ] { src = "/lamdera-logo-white.png", description = "Lamdera logo" }
              --         --
              --         -- -- , el [ centerX, Font.size 40 ] <| text "LΛMDERΛ"
              --         -- , el [ centerX, Font.size 16, Font.bold ] <| text "Coming soon"
              --         -- , row [ height (px 50) ] []
              --
              --         -- , el [ centerX, Font.size 16 ] <| text "αlphα"
              --         ]
              --     ]
              column [ padding 50, spacing 20, width fill ] content
            ]


padding_ l t r b =
    paddingEach { bottom = b, left = l, right = r, top = t }


white =
    fromHex "#fff"


blue =
    fromHex "#00a1ff"


red =
    fromHex "#ff634e"


fromHex : String -> Color
fromHex str =
    case hexToColor str of
        Ok col ->
            let
                x =
                    Color.toRgba col
            in
            Element.rgba x.red x.green x.blue x.alpha

        Err _ ->
            Element.rgb 255 0 0
