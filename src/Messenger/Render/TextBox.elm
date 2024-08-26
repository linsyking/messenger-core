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
  - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
  - pos is the position of the left-top corner of the textbox.
      - Note: The textbox is left-top aligned, which is different from text, whose default setting is left-buttom.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBoxWithStyle : InternalData -> Float -> String -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithStyle gd fontsize content ft style pos size =
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
  - fontsize is the size of the font in pounds.
  - content is the string you may want to display.
  - ft is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - pos is the position of the left-top corner of the textbox.
  - Note: The textbox is left-top aligned, which is different from text, whose default setting is left-buttom.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBoxWithColor : InternalData -> Float -> String -> String -> Color -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColor gd fontsize content font color pos size =
    renderTextBoxWithColorStyle gd fontsize content font color "" pos size


{-| Render colorful text box with style.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - fontsize is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
  - pos is the position of the left-top corner of the textbox.
      - Note: The textbox is left-top aligned, which is different from text, whose default setting is left-buttom.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBoxWithColorStyle : InternalData -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColorStyle gd fontsize content ft color style pos size =
    renderTextBoxWithAll gd fontsize content ft style pos size color "left" "top" "" "" Nothing False


{-| Render text box with color and align middle.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - fontsize is the size of the font in pounds.
  - content is the string you may want to display.
  - ft is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - pos is the position of the middle in both x and y axis of the textbox.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBoxWithColorCenter : InternalData -> Float -> String -> String -> Color -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColorCenter gd fontsize content font color pos size =
    renderTextBoxWithColorCenterStyle gd fontsize content font color "" pos size


{-| Render text box with color, align center and style.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - fontsize is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
  - pos is the position of the middle in both x and y axis of the textbox.
  - size is the size of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.

-}
renderTextBoxWithColorCenterStyle : InternalData -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithColorCenterStyle gd fontsize content ft color style pos size =
    renderTextBoxWithAll gd fontsize content ft style pos size color "center" "middle" "" "" Nothing False


{-| Render text box with all settings. For every string setting you can leave an empty string to set it to default value(The default option is in bold font)

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - fontsize is the size of the font in pounds.
  - content is the string you may want to display.
  - ft is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
  - pos is the position of the textbox regarding the selected align and baseline scheme.
  - (w,h) is the width and height of the textbox. If the total width of the word exceed the width of the textbox, it will start a new line.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - align is the align option of the display string.
      - Note: align controls the horizontal position of the displayed string in the textbox. The possible arguments are
          - "left" : The string is aligned to the left of the textbox.
          - "right" : The string is aligned to the right of the textbox.
          - **"center"** : The text is aligned to the center of textbox.
          - "start" : In English context, it is synonym to "left".
          - "end" : In English context, it is synonym to "right".
  - baseline is the baseline option of the display string. Note the setting here are based on experiment but not official document, so there might be error.
      - Note: baseline controls the vertical position of the displayed string in the textbox.d The possible arguments are:
          - "top" : - The text is aligned to the top of the textbox.
          - **"middle"** : The text is aligned to the middle the textbox.
          - "bottom" : The text is beneath all parts of the character (which causes a difference in, say, "q" to alphabetic settings.) -- (The given position is to the bottom of every part of the string with considering descenders)
  - variant has only two option: "normal" or "small-caps", which will display all lowercase letters in smaller uppercase letters.
  - weight is the boldness of the font. You may choose from "normal", "bold", "bolder", or "lighter", or specific boldness numbers, set the website above.
      - Note: some fonts may not support bold.
  - height is the line height of the textbox in pixel(not in ratio!). For example, if your fontsize is 50 and you want a 2x line height, you should insert 100 instead of 2. You may also set it to Nothing to apply the default line height.
  - justify is the bool controling whether the text should be justified, that is to say, should be spread automatically to fill the whole textbox horizontally. Set it to True to justify the textbox. It is not usable now, though.

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
        , justify = Just False
        }
