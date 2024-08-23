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

  - Note: To add a font in a messenger game, follow these steps:

1.  copy the font package (\*.ttf) into assets/misc folder
2.  edit public/style.css, adding these lines:

```css
@font-face {
  font-family: 'YourFontName';
  src: url(../assets/misc/your-font-file-name.ttf);
}
```

-}

import Canvas exposing (Renderable, text)
import Canvas.Settings exposing (Setting, fill)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color exposing (Color)
import Messenger.Base exposing (InternalData)
import Messenger.Coordinate.Coordinates exposing (lengthToReal, posToReal)


{-| Render Text. Black color, left top align.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - pos is the position of the left-top corner of the text.

-}
renderText : InternalData -> Float -> String -> String -> ( Float, Float ) -> Renderable
renderText gd size content font pos =
    renderTextWithStyle gd size content font "" pos


{-| Render Text. Black color, left top align with style.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - ft is the font type in String.
  - style is the basic setting of the font, for example, "italic bold" will set the font to be italic and bold. See more styles at here:
    [fonts](https://www.w3schools.com/cssref/pr_font_font.php) and here [sandbox](https://www.w3schools.com/cssref/playdemo.php?filename=playcss_font-weight).
      - Note: The style here is not completely equal to font-style. Instead, it is in the format of "style variant weight", whose sequence order cannot be switched.
          - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
          - variant has only one option: "normal" or "small-caps", which will display all lowercase letters in smaller uppercase letters.
          - weight is the boldness of the font. You may choose from "normal", "bold", "bolder", or "lighter", or specific boldness numbers, set the website above.
  - pos is the position of the left-top corner of the text.

-}
renderTextWithStyle : InternalData -> Float -> String -> String -> String -> ( Float, Float ) -> Renderable
renderTextWithStyle gd size content ft style ( x, y ) =
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
        content


{-| Render colorful texts.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - pos is the position of the left-top corner of the text.

-}
renderTextWithColor : InternalData -> Float -> String -> String -> Color -> ( Float, Float ) -> Renderable
renderTextWithColor gd size content font color position =
    renderTextWithColorStyle gd size content font color "" position


{-| Render colorful texts with style, or, the combination of renderTextWithColor and renderTextWithStyle.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - ft is the font type in String.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - style is the basic setting of the font, for example, "italic bold" will set the font to be italic and bold. See more styles at here:
    [fonts](https://www.w3schools.com/cssref/pr_font_font.php) and here [sandbox](https://www.w3schools.com/cssref/playdemo.php?filename=playcss_font-weight).
      - Note: The style here is not completely equal to font-style. Instead, it is in the format of "style variant weight", whose sequence order cannot be switched.
          - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
          - variant has only one option: "normal" or "small-caps", which will display all lowercase letters in smaller uppercase letters.
          - weight is the boldness of the font. You may choose from "normal", "bold", "bolder", or "lighter", or specific boldness numbers, set the website above.
  - pos is the position of the left-top corner of the text.

-}
renderTextWithColorStyle : InternalData -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> Renderable
renderTextWithColorStyle gd size content ft color style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        [ font { style = style, size = floor rx, family = ft }
        , align Start
        , fill color
        , baseLine Top
        ]
        ( dsx, dsy )
        content


{-| Render texts with color and align to the center.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - pos is the position of the MIDDLE-top corner of the text.

-}
renderTextWithColorCenter : InternalData -> Float -> String -> String -> Color -> ( Float, Float ) -> Renderable
renderTextWithColorCenter gd size content font color position =
    renderTextWithColorCenterStyle gd size content font color "" position


{-| Render texts with color and align with style.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - ft is the font type in String.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - style is the basic setting of the font, for example, "italic bold" will set the font to be italic and bold. See more styles at here:
    [fonts](https://www.w3schools.com/cssref/pr_font_font.php) and here [sandbox](https://www.w3schools.com/cssref/playdemo.php?filename=playcss_font-weight).
      - Note: The style here is not completely equal to font-style. Instead, it is in the format of "style variant weight", whose sequence order cannot be switched.
          - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
          - variant has only one option: "normal" or "small-caps", which will display all lowercase letters in smaller uppercase letters.
          - weight is the boldness of the font. You may choose from "normal", "bold", "bolder", or "lighter", or specific boldness numbers, set the website above.
  - pos is the position of the MIDDLE-buttom corner of the text.

-}
renderTextWithColorCenterStyle : InternalData -> Float -> String -> String -> Color -> String -> ( Float, Float ) -> Renderable
renderTextWithColorCenterStyle gd size content ft color style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        [ font { style = style, size = floor rx, family = ft }
        , align Center
        , fill color
        , baseLine Middle
        ]
        ( dsx, dsy )
        content


{-| Render texts with color, align and baseline.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
  - size is the size of the font in pounds.
  - content is the string you may want to display.
  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.
  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).
  - align is the align scheme used.
      - Note: align controls the horizontal relation between the displayed string and the given point. The possible arguments are:
          - Left : The text is aligned to the left -- (The given position is at the left of the string).
          - Right : The text is aligned to the right. -- (The given position is at the right of the string).
          - Center : The text is aligned to the Center. -- (The given position is at the middle of the string).
          - Start : In English context, it is synonym to Left.
          - End : In English context, it is synonym to Right.
  - baseline is the baseline option of the display string.
      - Note: baseline controls the vertical relation between the displayed string and the given point. The possible arguments are:
          - Top : - The text baseline is the top of the string -- (The given position is to the top of the string).
          - Hanging : The baseline is at the top of the string. -- (The given position is just at the top of the string).
          - Middle : The text baseline is in the middle of the string -- (The given position is at the middle of the string).
          - Alphabetic : Normal setting. -- (The given position is to the bottom of the string)
          - Ideographic : The text baseline is beneath all parts of the character (which causes a difference in, say, "q" to alphabetic settings.) -- (The given position is to the bottom of every part of the string without considering descenders)
          - Bottom : The text baseline is beneath all parts of the character (which causes a difference in, say, "q" to alphabetic settings.) -- (The given position is to the bottom of every part of the string with considering descenders)
  - pos is the position with regard to the desired align scheme.

-}
renderTextWithColorAlignBaseline : InternalData -> Float -> String -> String -> Color -> TextAlign -> TextBaseLine -> ( Float, Float ) -> Renderable
renderTextWithColorAlignBaseline gd size content font color align baseline position =
    renderTextWithColorAlignBaselineStyle gd size content font color align baseline "" position


{-| Render texts with color, align and baseline with style.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.

  - size is the size of the font in pounds.

  - content is the string you may want to display.

  - ft is the font type in String.

  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).

  - align is the align scheme used.
      - Note: align controls the horizontal relation between the displayed string and the given point. The possible arguments are:
          - Left : The text is aligned to the left -- (The given position is at the left of the string).
          - Right : The text is aligned to the right. -- (The given position is at the right of the string).
          - Center : The text is aligned to the Center. -- (The given position is at the middle of the string).
          - Start : In English context, it is synonym to Left.
          - End : In English context, it is synonym to Right.

  - baseline is the baseline option of the display string.
      - Note: baseline controls the vertical relation between the displayed string and the given point. The possible arguments are:
          - Top : - The text baseline is the top of the string -- (The given position is to the top of the string).
          - Hanging : The baseline is at the top of the string. -- (The given position is just at the top of the string).
          - Middle : The text baseline is in the middle of the string -- (The given position is at the middle of the string).
          - Alphabetic : Normal setting. -- (The given position is to the bottom of the string)
          - Ideographic : The text baseline is beneath all parts of the character (which causes a difference in, say, "q" to alphabetic settings.) -- (The given position is to the bottom of every part of the string without considering descenders)
          - Bottom : The text baseline is beneath all parts of the character (which causes a difference in, say, "q" to alphabetic settings.) -- (The given position is to the bottom of every part of the string with considering descenders)

  - style is the basic setting of the font, for example, "italic bold" will set the font to be italic and bold. See more styles at here:
    [fonts](https://www.w3schools.com/cssref/pr_font_font.php) and here [sandbox](https://www.w3schools.com/cssref/playdemo.php?filename=playcss_font-weight).
      - Note: The style here is not completely equal to font-style. Instead, it is in the format of "style variant weight", whose sequence order cannot be switched.
          - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
          - variant has only one option: "normal" or "small-caps", which will display all lowercase letters in smaller uppercase letters.
          - weight is the boldness of the font. You may choose from "normal", "bold", "bolder", or "lighter", or specific boldness numbers, set the website above.

  - pos is the position with regard to the desired align scheme.

-}
renderTextWithColorAlignBaselineStyle : InternalData -> Float -> String -> String -> Color -> TextAlign -> TextBaseLine -> String -> ( Float, Float ) -> Renderable
renderTextWithColorAlignBaselineStyle gd size content ft color al bl style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        [ font { style = style, size = floor rx, family = ft }
        , align al
        , fill color
        , baseLine bl
        ]
        ( dsx, dsy )
        content


{-| Use customized settings to render texts.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.

  - size is the size of the font in pounds.

  - content is the string you may want to display.

  - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.

  - settings is the list of Canvas.Settings. See Canvas.Setting package for more detail.

  - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).

-}
renderTextWithSettings : InternalData -> Float -> String -> String -> List Setting -> ( Float, Float ) -> Renderable
renderTextWithSettings gd size content font settings pos =
    renderTextWithSettingsStyle gd size content font settings "" pos


{-| Use customized settings to render texts with style.

  - gd is the internal data. You should put env.globalData.internalData as the first parameter to the function.
      - size is the size of the font in pounds.

      - content is the string you may want to display.

      - font is the name of the font. You can choose to use a font in Google font, or put your font file in the misc folder.

      - settings is the list of Canvas.Settings. See Canvas.Setting package for more detail.

      - color is a color type object. See Color elm package for more detail. For example, yellow or (Color.rgb255 17 232 234).

      - style is the basic setting of the font, for example, "italic bold" will set the font to be italic and bold. See more styles at here:
        [fonts](https://www.w3schools.com/cssref/pr_font_font.php) and here [sandbox](https://www.w3schools.com/cssref/playdemo.php?filename=playcss_font-weight).
          - Note: The style here is not completely equal to font-style. Instead, it is in the format of "style variant weight", whose sequence order cannot be switched.
              - style is the style of the font. You could use "normal", "italic" or "oblique". Note italic is slight more handwriting, but oblique is just the slanted version of the normal font.
              - variant has only one option: "normal" or "small-caps", which will display all lowercase letters in smaller uppercase letters.
              - weight is the boldness of the font. You may choose from "normal", "bold", "bolder", or "lighter", or specific boldness numbers, set the website above.

-}
renderTextWithSettingsStyle : InternalData -> Float -> String -> String -> List Setting -> String -> ( Float, Float ) -> Renderable
renderTextWithSettingsStyle gd size content ft settings style ( x, y ) =
    let
        rx =
            lengthToReal gd size

        ( dsx, dsy ) =
            posToReal gd ( x, y )
    in
    text
        (font { style = style, size = floor rx, family = ft } :: settings)
        ( dsx, dsy )
        content
