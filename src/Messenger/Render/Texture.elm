module Messenger.Render.Texture exposing (textureDim)

{-|


# Texture Rendering

@docs textureDim

-}

import Messenger.Base exposing (InternalData)
import Messenger.Resources.Base exposing (igetSprite)


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
