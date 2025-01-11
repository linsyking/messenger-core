module Messenger.Render.Texture exposing
    ( renderSprite
    , textureDim
    )

{-|


# Texture Rendering

@docs renderSprite
@docs textureDim

-}

import Messenger.Base exposing (InternalData)
import Messenger.Resources.Base exposing (igetSprite)
import REGL exposing (Renderable, Texture)
import REGL.BuiltinPrograms as P


{-| Render a sprite with the size automatically calculated from the texture.
-}
renderSprite : InternalData -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
renderSprite gd position size name =
    let
        dst =
            gd.sprites
    in
    case igetSprite name dst of
        Just t ->
            renderSpriteAutoHelper position size t

        Nothing ->
            REGL.empty


renderSpriteAutoHelper : ( Float, Float ) -> ( Float, Float ) -> Texture -> Renderable
renderSpriteAutoHelper ( newx, newy ) ( w, h ) t =
    if w /= 0 && h /= 0 then
        P.rectTexture ( newx, newy ) ( w, h ) t.name

    else if w /= 0 && h == 0 then
        P.rectTexture ( newx, newy ) ( w, w / toFloat t.width * toFloat t.height ) t.name

    else if w == 0 && h /= 0 then
        P.rectTexture ( newx, newy ) ( h / toFloat t.height * toFloat t.width, h ) t.name

    else
        -- All == 0
        P.rectTexture ( newx, newy ) ( toFloat t.width, toFloat t.height ) t.name


{-| Get the width and height of a texture.
-}
textureDim : InternalData -> String -> ( Int, Int )
textureDim gd name =
    Maybe.withDefault
        ( 0, 0 )
    <|
        Maybe.map
            (\t ->
                ( t.width, t.height )
            )
            (igetSprite name gd.sprites)
