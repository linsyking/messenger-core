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

Normally, users do not need to use this module directly, as Messenger will handle the coordinate transformation for you.

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


{-| Same as posToReal, but add the initial position of canvas. That is to say, the function returns the actual coordinate of the point **on the whole screen** of your computer, so it is dependent on the location of your browser windows.

Example: If your screen is 2560\*1440, the virtual canvas size is 1920\*1080 and the actual window size 960\*540 at the center of screen, then the function will map (600,300) to (2560/2-960/2+(960/1920)\*600,1440/2-540/2+(540/1080)\*300) = (1100,600)

-}
fixedPosToReal : InternalData -> ( Float, Float ) -> ( Float, Float )
fixedPosToReal gd ( x, y ) =
    floatpairadd (posToReal gd ( x, y )) ( gd.startLeft, gd.startTop )


{-| Transform from the virtual coordinate system to the real pixel system.

For example, if you virtual canvas is 1920\*1080 but your real window is 960\*540, then the function will map a point at (1200,600) to (600,300).

Usage : `posToReal gd ( x, y )` where (x,y) is the virtual coordinates and gd the internal data.

The function returns the position in real canvas.

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


{-| Inverse of posToReal. This is helpful if you need to render sprite with real coordinates.

Usage : `posToReal gd ( x, y )` where (x,y) is the real coordinates and gd the internal data.
The function returns the coordinate in virtual coordinate system.

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


{-| Use this if you want to draw something based on the length. It turns the virtual length to real length.
For example, if the actual window is 960\*540 and the virtual one is 1920\*1080, then the function will map 300(virtual length) to 150(real length)

  - Note: In the case the actual width-length ratio of the window is different from the virtual ratio, the zooming ratio is calculated based on the WIDTH ratio between the real and virtual canvas size.

Usage: `lengthToReal gd x`

  - gd is the internal data.
  - x is the length in virtual coordinate system.

The function returns the length in real coordinate system.

-}
lengthToReal : InternalData -> Float -> Float
lengthToReal gd x =
    gd.realWidth * (x / gd.virtualWidth)


{-| The inverse function of widthToReal. Turns the length in real length to virtual length.

  - Note: In the case the actual width-length ratio of the window is different from the virtual ratio, the zooming ratio is calculated based on the WIDTH ratio between the real and virtual canvas size.
    Usage: `lengthToReal gd x`

  - gd is the internal data.

  - x is the length in real coordinate system.

The function returns the length in virtual coordinate system.

-}
fromRealLength : InternalData -> Float -> Float
fromRealLength gd x =
    gd.virtualWidth * (x / gd.realWidth)


{-| Used internally.
-}
maxHandW : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
maxHandW vsize ( w, h ) =
    if w / h > plScale vsize then
        ( h * plScale vsize, h )

    else
        ( w, w / plScale vsize )


{-| Get the start point of the virtual canvas in the real canvas.

Used internally.

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
Usage: `judgeMouseRect ( mx, my ) ( x, y ) ( w, h )`

  - (mx,my) is the coordinate of the mouse, which you can get easily from globalData.
  - (x,y) is the starting point(left-top corner) of the desired rectangle.
  - (w,h) is the width and length of the desired rectangle.

The function returns a bool indicating whether the mouse is in the rectangle.

-}
judgeMouseRect : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Bool
judgeMouseRect ( mx, my ) ( x, y ) ( w, h ) =
    x <= mx && mx <= x + w && y <= my && my <= y + h


{-| fromMouseToVirtual conveys the mouse position in the Screen coordinate to the virtual coordinate. In most cases Messenger does this for you.

The coordinate in the globalData is already in virtual coordinates.

-}
fromMouseToVirtual : InternalData -> ( Float, Float ) -> ( Float, Float )
fromMouseToVirtual gd ( px, py ) =
    posToVirtual gd ( px - gd.startLeft, py - gd.startTop )
