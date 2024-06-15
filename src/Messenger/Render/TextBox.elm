module Messenger.Render.TextBox exposing
    ( renderTextBox, renderTextBoxWithStyle
    , renderTextBoxWithColor, renderTextBoxWithColorStyle
    , renderTextBoxWithColorCenter, renderTextBoxWithColorCenterStyle
    , renderTextBoxWithColorAlignBaseline, renderTextBoxWithColorAlignBaselineStyle
    , renderTextBoxWithSettings, renderTextBoxWithSettingsStyle
    )

{-|


# Text Box Rendering

@docs renderTextBox, renderTextBoxWithStyle
@docs renderTextBoxWithColor, renderTextBoxWithColorStyle
@docs renderTextBoxWithColorCenter, renderTextBoxWithColorCenterStyle
@docs renderTextBoxWithColorAlignBaseline, renderTextBoxWithColorAlignBaselineStyle
@docs renderTextBoxWithSettings, renderTextBoxWithSettingsStyle

-}

import Canvas exposing (Renderable)
import Canvas.Settings exposing (Setting)
import Canvas.Settings.Text exposing (TextAlign, TextBaseLine, align, font)
import Color exposing (Color)
import Messenger.Base exposing (GlobalData)
import Messenger.Render.Text exposing (renderTextWithColorAlignBaselineStyle, renderTextWithColorCenterStyle, renderTextWithColorStyle, renderTextWithSettingsStyle, renderTextWithStyle)


{-| Turn a string to list of text.
-}
box : String -> ( Float, Float ) -> Float -> (String -> ( Float, Float ) -> Renderable) -> Renderable
box content ( startx, starty ) size f =
    let
        spiltText =
            String.split "\n" content
    in
    Canvas.group [] <|
        List.indexedMap (\idx single -> f single ( startx, starty + size * toFloat idx )) spiltText


{-| Render Text Box. Black color, left top align.
-}
renderTextBox : GlobalData a -> Float -> String -> String -> ( Float, Float ) -> Renderable
renderTextBox gd size content font pos =
    renderTextBoxWithStyle gd size content font "" pos


{-| Render Text Box. Black color, left top align with style.
-}
renderTextBoxWithStyle : GlobalData a -> Float -> String -> String -> String -> ( Float, Float ) -> Renderable
renderTextBoxWithStyle gd size content ft style pos =
    box content pos size (\s posr -> renderTextWithStyle gd size s ft style posr)


{-| Render colorful text box.
-}
renderTextBoxWithColor : GlobalData a -> Float -> String -> String -> Color -> ( Float, Float ) -> Renderable
renderTextBoxWithColor gd size content font color position =
    renderTextBoxWithColorStyle gd size content font color "" position


{-| Render colorful text box with style.
-}
renderTextBoxWithColorStyle : GlobalData a -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> Renderable
renderTextBoxWithColorStyle gd size content ft color style pos =
    box content pos size (\s posr -> renderTextWithColorStyle gd size s ft color style posr)


{-| Render text box with color and align.
-}
renderTextBoxWithColorCenter : GlobalData a -> Float -> String -> String -> Color -> ( Float, Float ) -> Renderable
renderTextBoxWithColorCenter gd size content font color position =
    renderTextBoxWithColorCenterStyle gd size content font color "" position


{-| Render text box with color, align and style.
-}
renderTextBoxWithColorCenterStyle : GlobalData a -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> Renderable
renderTextBoxWithColorCenterStyle gd size content ft color style pos =
    box content pos size (\s posr -> renderTextWithColorCenterStyle gd size s ft color style posr)


{-| Render texts with color, align and baseline.
-}
renderTextBoxWithColorAlignBaseline : GlobalData a -> Float -> String -> String -> Color -> TextAlign -> TextBaseLine -> ( Float, Float ) -> Renderable
renderTextBoxWithColorAlignBaseline gd size content font color align baseline position =
    renderTextBoxWithColorAlignBaselineStyle gd size content font color align baseline "" position


{-| Render texts with color, align and baseline with style.
-}
renderTextBoxWithColorAlignBaselineStyle : GlobalData a -> Float -> String -> String -> Color -> TextAlign -> TextBaseLine -> String -> ( Float, Float ) -> Renderable
renderTextBoxWithColorAlignBaselineStyle gd size content ft color al bl style pos =
    box content pos size (\s posr -> renderTextWithColorAlignBaselineStyle gd size s ft color al bl style posr)


{-| Use customized settings to render text box.
-}
renderTextBoxWithSettings : GlobalData a -> Float -> String -> String -> List Setting -> ( Float, Float ) -> Renderable
renderTextBoxWithSettings gd size content font settings pos =
    renderTextBoxWithSettingsStyle gd size content font settings "" pos


{-| Use customized settings to render text box with style.
-}
renderTextBoxWithSettingsStyle : GlobalData a -> Float -> String -> String -> List Setting -> String -> ( Float, Float ) -> Renderable
renderTextBoxWithSettingsStyle gd size content ft settings style pos =
    box content pos size (\s posr -> renderTextWithSettingsStyle gd size s ft settings style posr)
