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

import Messenger.Base exposing (GlobalData)



{- The scale is by default 16:9 -}


plScale : GlobalData a -> Float
plScale gd =
    gd.internalData.virtualWidth / gd.internalData.virtualHeight



--- Transform Coordinates


floatpairadd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
floatpairadd ( x, y ) ( z, w ) =
    ( x + z, y + w )


{-| fixedPosToReal

Same as posToReal, but add the initial position of canvas.

-}
fixedPosToReal : GlobalData a -> ( Float, Float ) -> ( Float, Float )
fixedPosToReal gd ( x, y ) =
    floatpairadd (posToReal gd ( x, y )) ( gd.internalData.startLeft, gd.internalData.startTop )


{-| posToReal

Transform from the virtual coordinate system to the real pixel system.

-}
posToReal : GlobalData a -> ( Float, Float ) -> ( Float, Float )
posToReal gd ( x, y ) =
    let
        realWidth =
            gd.internalData.realWidth

        realHeight =
            gd.internalData.realHeight
    in
    ( realWidth * (x / gd.internalData.virtualWidth), realHeight * (y / gd.internalData.virtualHeight) )


{-| Inverse of posToReal.
-}
posToVirtual : GlobalData a -> ( Float, Float ) -> ( Float, Float )
posToVirtual gd ( x, y ) =
    let
        realWidth =
            gd.internalData.realWidth

        realHeight =
            gd.internalData.realHeight
    in
    ( gd.internalData.virtualWidth * (x / realWidth), gd.internalData.virtualHeight * (y / realHeight) )


{-| widthToReal

Use this if you want to draw something based on the length.

-}
lengthToReal : GlobalData a -> Float -> Float
lengthToReal gd x =
    gd.internalData.realWidth * (x / gd.internalData.virtualWidth)


{-| The inverse function of widthToReal.
-}
fromRealLength : GlobalData a -> Float -> Float
fromRealLength gd x =
    gd.internalData.virtualWidth * (x / gd.internalData.realWidth)


{-| maxHandW
-}
maxHandW : GlobalData a -> ( Float, Float ) -> ( Float, Float )
maxHandW gd ( w, h ) =
    if w / h > plScale gd then
        ( h * plScale gd, h )

    else
        ( w, w / plScale gd )


{-| getStartPoint
-}
getStartPoint : GlobalData a -> ( Float, Float ) -> ( Float, Float )
getStartPoint gd ( w, h ) =
    let
        fw =
            h * plScale gd

        fh =
            w / plScale gd
    in
    if w / h > plScale gd then
        ( (w - fw) / 2, 0 )

    else
        ( 0, (h - fh) / 2 )


{-| judgeMouseRect
Judge whether the mouse position is in the rectangle.
-}
judgeMouseRect : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Bool
judgeMouseRect ( mx, my ) ( x, y ) ( w, h ) =
    if x <= mx && mx <= x + w && y <= my && my <= y + h then
        True

    else
        False


{-| fromMouseToVirtual
-}
fromMouseToVirtual : GlobalData a -> ( Float, Float ) -> ( Float, Float )
fromMouseToVirtual gd ( px, py ) =
    posToVirtual gd ( px - gd.internalData.startLeft, py - gd.internalData.startTop )
