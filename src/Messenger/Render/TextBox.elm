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
-}
renderTextBox : InternalData -> Float -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBox gd size content font =
    renderTextBoxWithStyle gd size content font ""


{-| Render text box with line height.
-}
renderTextBoxWithLineHeight : InternalData -> Float -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Float -> Renderable
renderTextBoxWithLineHeight gd fontsize content ft pos size lheight =
    renderTextBoxWithAll gd fontsize content ft "" pos size Color.black "left" "top" "" "" (Just lheight) False


{-| Render text box. Black color, left top align with style.
-}
renderTextBoxWithStyle : InternalData -> Float -> String -> String -> String -> ( Float, Float ) -> ( Float, Float ) -> Renderable
renderTextBoxWithStyle gd fontsize content ft style pos size =
    renderTextBoxWithAll gd fontsize content ft style pos size Color.black "left" "top" "" "" Nothing False


{-| Render colorful text box.
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
