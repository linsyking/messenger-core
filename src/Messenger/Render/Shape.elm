module Messenger.Render.Shape exposing (circle, rect)

{-|


# Shape Rendering

@docs circle, rect

-}

import Canvas
import Messenger.Base exposing (InternalData)
import Messenger.Coordinate.Coordinates exposing (lengthToReal, posToReal)


{-| Render a circle.

Usage: circle gd pos r

  - gd is the internal data type. You should always put globaldata.internaldata as your first parameter.
  - The pos is the center of the circle in virtual coordinates.
  - r is the radius of the circle (in virtual length).
      - Note: The renderSprite automatically does posToReal and lengthToReal for you. No need to call them again.

-}
circle : InternalData -> ( Float, Float ) -> Float -> Canvas.Shape
circle gd pos r =
    Canvas.circle (posToReal gd pos) (lengthToReal gd r)


{-| Render a rectangle.

Usage: rect gd pos ( w, h )

  - gd is the internal data type. You should always put globaldata.internaldata as your first parameter.
  - The pos is the start point of the rectangle from the left-top corner in virtual coordinates.
      - Note: The renderSprite automatically does posToReal and lengthToReal for you. No need to call them again.
  - (w,h) is the size of the sprite. W refers to the width and h refers to height(all in virtual coordinate).

-}
rect : InternalData -> ( Float, Float ) -> ( Float, Float ) -> Canvas.Shape
rect gd pos ( w, h ) =
    Canvas.rect (posToReal gd pos) (lengthToReal gd w) (lengthToReal gd h)
