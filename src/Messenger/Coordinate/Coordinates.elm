module Messenger.Coordinate.Coordinates exposing
    ( fixedPosToReal
    , posToReal, posToVirtual
    , lengthToReal
    , fromRealLength
    , maxHandW
    , getStartPoint
    , judgeMouseRect
    , fromMouseToVirtual
    )

{-|


# Coordinate

This module deals with the coordinate transformation.

This module is very important because it can calculate the correct position of the point you want to draw.

@docs fixedPosToReal
@docs posToReal, posToVirtual
@docs lengthToReal
@docs fromRealLength
@docs maxHandW
@docs getStartPoint
@docs judgeMouseRect
@docs fromMouseToVirtual

-}

import Messenger.Base exposing (InternalData)


plScale : ( Float, Float ) -> Float
plScale ( vw, vh ) =
    vw / vh



--- Transform Coordinates


floatpairadd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
floatpairadd ( x, y ) ( z, w ) =
    ( x + z, y + w )


{-| fixedPosToReal

Same as posToReal, but add the initial position of canvas.

-}
fixedPosToReal : InternalData -> ( Float, Float ) -> ( Float, Float )
fixedPosToReal gd ( x, y ) =
    floatpairadd (posToReal gd ( x, y )) ( gd.startLeft, gd.startTop )


{-| posToReal

Transform from the virtual coordinate system to the real pixel system.

-}
posToReal : InternalData -> ( Float, Float ) -> ( Float, Float )
posToReal gd ( x, y ) =
    let
        realWidth =
            gd.realWidth

        realHeight =
            gd.realHeight
    in
    ( realWidth * (x / gd.virtualWidth), realHeight * (y / gd.virtualHeight) )


{-| Inverse of posToReal.
-}
posToVirtual : InternalData -> ( Float, Float ) -> ( Float, Float )
posToVirtual gd ( x, y ) =
    let
        realWidth =
            gd.realWidth

        realHeight =
            gd.realHeight
    in
    ( gd.virtualWidth * (x / realWidth), gd.virtualHeight * (y / realHeight) )


{-| widthToReal

Use this if you want to draw something based on the length.

-}
lengthToReal : InternalData -> Float -> Float
lengthToReal gd x =
    gd.realWidth * (x / gd.virtualWidth)


{-| The inverse function of widthToReal.
-}
fromRealLength : InternalData -> Float -> Float
fromRealLength gd x =
    gd.virtualWidth * (x / gd.realWidth)


{-| maxHandW
-}
maxHandW : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
maxHandW vsize ( w, h ) =
    if w / h > plScale vsize then
        ( h * plScale vsize, h )

    else
        ( w, w / plScale vsize )


{-| getStartPoint
-}
getStartPoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
getStartPoint vsize ( w, h ) =
    let
        fw =
            h * plScale vsize

        fh =
            w / plScale vsize
    in
    if w / h > plScale vsize then
        ( (w - fw) / 2, 0 )

    else
        ( 0, (h - fh) / 2 )


{-| judgeMouseRect
Judge whether the mouse position is in the rectangle.
-}
judgeMouseRect : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Bool
judgeMouseRect ( mx, my ) ( x, y ) ( w, h ) =
    x <= mx && mx <= x + w && y <= my && my <= y + h


{-| fromMouseToVirtual
-}
fromMouseToVirtual : InternalData -> ( Float, Float ) -> ( Float, Float )
fromMouseToVirtual gd ( px, py ) =
    posToVirtual gd ( px - gd.startLeft, py - gd.startTop )
