module Messenger.Render.TextBox exposing
    ( renderTextBox, renderTextBoxWithStyle
    , renderTextBoxWithColor, renderTextBoxWithColorStyle
    , renderTextBoxWithColorCenter, renderTextBoxWithColorCenterStyle
    , renderTextBoxWithAll, renderTextBoxWithLineHeight
    )

{-|


# Text box Rendering

@docs renderTextBox, renderTextBoxWithStyle
@docs renderTextBoxWithColor, renderTextBoxWithColorStyle
@docs renderTextBoxWithColorCenter, renderTextBoxWithColorCenterStyle
@docs renderTextBoxWithAll, renderTextBoxWithLineHeight

-}

import Canvas exposing (Renderable, textbox)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Text exposing (align, font)
import Color exposing (Color)
import Messenger.Base exposing (InternalData)
import Messenger.Coordinate.Coordinates exposing (lengthToReal, posToReal)


{-| Render text box. Black color, left top align.
Textbox is similar to Text, but it allows the words to start a new line automatically when needed.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - pos is the position of the left-top corner of the textbox.
      - Note: The textbox is left-top aligned, which is different from text, whose default setting is left-buttom.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBox : InternalData -> Float -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBox gd fontsize content font pos size =
    renderTextBoxWithStyle gd fontsize content font "" pos size


{-| Render text box with line height.
Textbox is similar to Text, but it allows the words to start a new line automatically when needed.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - pos is the position of the left-top corner of the textbox.
      - Note: The textbox is left-top aligned, which is different from text, whose default setting is left-buttom.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.
  - lheight is the line height of the textbox in pixel(not in ratio!). For example, if your fontsize is 50 and you want a 2x line height, you should insert 100 instead of 2.

-}
renderTextBoxWithLineHeight : InternalData -> Float -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Float -> Renderable
renderTextBoxWithLineHeight gd fontsize content ft pos size lheight =
    renderTextBoxWithAll gd fontsize content ft "" pos size Color.black "left" "top" "" "" (Just lheight) False


{-| Render text box. Black color, left top align with style.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - ft is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - style is the basic setting of the font, for example, "italic bold" will set the font to be italic and bold. See more styles at here:
    [fonts](https://www.w3schools.com/cssref/pr_font_font.php) and here [sandbox](https://www.w3schools.com/cssref/playdemo.php?filename=playcss_font-weight).
      - Note: The style here is not completely equal to font-style. Instead, it is in the format of "style variant weight", whose sequence order cannot be switched.
          - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
          - variant has only one option: "normal" or "small-caps", which will display all lowercase letters in smaller uppercase letters.
          - weight is the boldness of the font. You may choose from "normal", "bold", "bolder", or "lighter", or specific boldness numbers, set the website above.
  - pos is the position of the left-top corner of the textbox.
      - Note: The textbox is left-top aligned, which is different from text, whose default setting is left-buttom.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBoxWithStyle : InternalData -> Float -> String -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithStyle gd fontsize content ft style pos size =
    S
        renderTextBoxWithAll
        gd
        fontsize
        content
        ft
        style
        pos
        size
        Color.black
        "left"
        "top"
        ""
        ""
        Nothing
        False


{-| Render colorful text box.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
      - size is the size of the font in pounds.
      - content is the string you may want to display.
      - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
      - pos is the position of the left-top corner of the textbox.
          - Note: The textbox is left-top aligned, which is different from text, whose default setting is left-buttom.
      - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBoxWithColor : InternalData -> Float -> String -> String -> Color -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColor gd size content font color =
    renderTextBoxWithColorStyle gd size content font color ""


{-| Render colorful text box with style.
-}
renderTextBoxWithColorStyle : InternalData -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColorStyle gd fontsize content ft color style pos size =
    renderTextBoxWithAll gd fontsize content ft style pos size color "left" "top" "" "" Nothing False


{-| Render text box with color and align.
-}
renderTextBoxWithColorCenter : InternalData -> Float -> String -> String -> Color -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColorCenter gd size content font color =
    renderTextBoxWithColorCenterStyle gd size content font color ""


{-| Render text box with color, align and style.
-}
renderTextBoxWithColorCenterStyle : InternalData -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColorCenterStyle gd fontsize content ft color style pos size =
    renderTextBoxWithAll gd fontsize content ft style pos size color "center" "middle" "" "" Nothing False


{-| Render text box with all settings.
-}
renderTextBoxWithAll : InternalData -> Float -> String -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Color -> String -> String -> String -> String -> Maybe Float -> Bool -> Renderable
renderTextBoxWithAll gd fontsize content ft style pos ( w, h ) color align baseline variant weight height justify =
    let
        rx =
            lengthToReal gd fontsize

        rp =
            posToReal gd pos

        rsize =
            ( lengthToReal gd w, lengthToReal gd h )
    in
    textbox [ fill color ]
        { point = rp
        , size = rsize
        , text = content
        , align = Just align
        , baseLine = Just baseline
        , fontSize = Just rx
        , font = Just ft
        , fontStyle = Just style
        , fontVariant = Just variant
        , fontWeight = Just weight
        , lineHeight = height
        , justify = Just justify
        }
