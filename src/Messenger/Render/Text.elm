module Messenger.Render.Text exposing
    ( renderText, renderTextWithStyle
    , renderTextWithColor, renderTextWithColorStyle
    , renderTextWithColorCenter, renderTextWithColorCenterStyle
    , renderTextWithColorAlignBaseline, renderTextWithColorAlignBaselineStyle
    , renderTextWithSettings, renderTextWithSettingsStyle
    )

{-|


# Text Rendering

@docs renderText, renderTextWithStyle
@docs renderTextWithColor, renderTextWithColorStyle
@docs renderTextWithColorCenter, renderTextWithColorCenterStyle
@docs renderTextWithColorAlignBaseline, renderTextWithColorAlignBaselineStyle
@docs renderTextWithSettings, renderTextWithSettingsStyle

-}

import Canvas exposing (Renderable, text)
import Canvas.Settings exposing (Setting, fill)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color exposing (Color)
import Messenger.Base exposing (GlobalData)
import Messenger.Coordinate.Coordinates exposing (lengthToReal, posToReal)


{-| Render Text. Black color, left top align.
-}
renderText : GlobalData a -> Float -> String -> String -> ( Float, Float ) -> Renderable
renderText gd size s ft pos =
    renderTextWithStyle gd size s ft "" pos


{-| Render Text. Black color, left top align.
-}
renderTextWithStyle : GlobalData a -> Float -> String -> String -> String -> ( Float, Float ) -> Renderable
renderTextWithStyle gd size s ft style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        [ font { style = style, size = floor rx, family = ft }
        , align Start
        , fill Color.black
        , baseLine Top
        ]
        ( dsx, dsy )
        s


{-| renderTextWithColor
Render colorful texts.
-}
renderTextWithColor : GlobalData a -> Float -> String -> String -> Color -> ( Float, Float ) -> Renderable
renderTextWithColor gd size s ft col pos =
    renderTextWithColorStyle gd size s ft col "" pos


{-| renderTextWithColor
Render colorful texts.
-}
renderTextWithColorStyle : GlobalData a -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> Renderable
renderTextWithColorStyle gd size s ft col style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        [ font { style = style, size = floor rx, family = ft }
        , align Start
        , fill col
        , baseLine Top
        ]
        ( dsx, dsy )
        s


{-| renderTextWithColorAlign
Render texts with color and align.
-}
renderTextWithColorCenter : GlobalData a -> Float -> String -> String -> Color -> ( Float, Float ) -> Renderable
renderTextWithColorCenter gd size s ft col pos =
    renderTextWithColorCenterStyle gd size s ft col "" pos


{-| renderTextWithColorAlign
Render texts with color and align.
-}
renderTextWithColorCenterStyle : GlobalData a -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> Renderable
renderTextWithColorCenterStyle gd size s ft col style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        [ font { style = style, size = floor rx, family = ft }
        , align Center
        , fill col
        , baseLine Middle
        ]
        ( dsx, dsy )
        s


{-| Render texts with color, align and baseline.
-}
renderTextWithColorAlignBaseline : GlobalData a -> Float -> String -> String -> Color -> TextAlign -> TextBaseLine -> ( Float, Float ) -> Renderable
renderTextWithColorAlignBaseline gd size s ft col al bl pos =
    renderTextWithColorAlignBaselineStyle gd size s ft col al bl "" pos


{-| Render texts with color, align and baseline.
-}
renderTextWithColorAlignBaselineStyle : GlobalData a -> Float -> String -> String -> Color -> TextAlign -> TextBaseLine -> String -> ( Float, Float ) -> Renderable
renderTextWithColorAlignBaselineStyle gd size s ft col al bl style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        [ font { style = style, size = floor rx, family = ft }
        , align al
        , fill col
        , baseLine bl
        ]
        ( dsx, dsy )
        s


{-| Use customized settings to render texts.
-}
renderTextWithSettings : GlobalData a -> Float -> String -> String -> List Setting -> ( Float, Float ) -> Renderable
renderTextWithSettings gd size str ft settings pos =
    renderTextWithSettingsStyle gd size str ft settings "" pos


{-| Use customized settings to render texts.
-}
renderTextWithSettingsStyle : GlobalData a -> Float -> String -> String -> List Setting -> String -> ( Float, Float ) -> Renderable
renderTextWithSettingsStyle gd size str ft settings style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        (font { style = style, size = floor rx, family = ft } :: settings)
        ( dsx, dsy )
        str
