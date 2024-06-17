module Messenger.Render.Shape exposing (circle, rect)

{-|


# Shape Rendering

@docs circle, rect

-}

import Canvas
import Messenger.Base exposing (InternalData)
import Messenger.Coordinate.Coordinates exposing (lengthToReal, posToReal)


{-| Draw circle based on global data.
-}
circle : InternalData -> ( Float, Float ) -> Float -> Canvas.Shape
circle gd pos r =
    Canvas.circle (posToReal gd pos) (lengthToReal gd r)


{-| Draw rectangle based on global data.
-}
rect : InternalData -> ( Float, Float ) -> ( Float, Float ) -> Canvas.Shape
rect gd pos ( w, h ) =
    Canvas.rect (posToReal gd pos) (lengthToReal gd w) (lengthToReal gd h)
