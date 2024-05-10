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
-}
type alias SpriteSheet =
    Dict String (List ( String, SingleSprite ))
