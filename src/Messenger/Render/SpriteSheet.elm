module Messenger.Render.SpriteSheet exposing
    ( SingleSprite
    , SpriteSheet
    )

{-|


# Sprite Sheet

@docs SingleSprite
@docs SpriteSheet

-}

import Dict exposing (Dict)


{-| A Single Sprite in a SpriteSheet

The unit of realSize is pixel.

-}
type alias SingleSprite =
    { realStartPoint : ( Float, Float ) -- (x, y)
    , realSize : ( Float, Float ) -- (Width, Height)
    }


{-| SpriteSheet

Users should both name the sprite sheets and every single sprite.
Using it by **format: "sheet\_name.sprite\_name"**

Sprite sheets are useful when managing the art resourses or making frame-by-frame animations.

-}
type alias SpriteSheet =
    Dict String (List ( String, SingleSprite ))
