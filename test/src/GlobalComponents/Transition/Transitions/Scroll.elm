module GlobalComponents.Transition.Transitions.Scroll exposing
    ( scrollIn, scrollInStorage
    , scrollOut, scrollOutStorage
    )

{-| Scroll Transition

@docs scrollIn, scrollInStorage
@docs scrollOut, scrollOutStorage

-}

import Canvas exposing (group, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (GlobalCompositeOperationMode(..), compositeOperationMode, fillLinear)
import Color exposing (Color)
import GlobalComponents.Transition.Transitions.Base exposing (SingleTrans, TransStorage, colorDec, colorEnc)
import Messenger.Coordinate.Coordinates exposing (lengthToReal)
import Messenger.Render.Shape exposing (rect)
import Messenger.Scene.Scene exposing (GCMsg)


{-| Scroll Out
-}
scrollOut : Color -> ( String, GCMsg )
scrollOut col =
    ( "ScrollOut", colorEnc col )


{-| ScrollOut Storage
-}
scrollOutStorage : TransStorage
scrollOutStorage msg =
    scrollOutHelper (colorDec msg)


scrollOutHelper : Color -> SingleTrans
scrollOutHelper col gd rd v =
    group []
        [ rd
        , shapes
            [ fillLinear { x0 = 0, y0 = 0, x1 = lengthToReal gd gd.virtualWidth, y1 = 0 }
                [ ( 0, Color.rgba 0 0 0 0 )
                , ( if 0.95 - v >= 0 then
                        0.95 - v

                    else
                        0
                  , Color.rgba 0 0 0 0
                  )
                , ( 1 - v, Color.rgba 0 0 0 1 )
                , ( 1, Color.rgba 0 0 0 1 )
                ]
            , compositeOperationMode DestinationOut
            ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        , shapes
            [ fill col, compositeOperationMode DestinationOver ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        ]


{-| Scroll In
-}
scrollIn : Color -> ( String, GCMsg )
scrollIn col =
    ( "ScrollIn", colorEnc col )


{-| ScrollIn Storage
-}
scrollInStorage : TransStorage
scrollInStorage msg =
    scrollInHelper (colorDec msg)


scrollInHelper : Color -> SingleTrans
scrollInHelper col gd rd v =
    group []
        [ rd
        , shapes
            [ fillLinear { x0 = 0, y0 = 0, x1 = lengthToReal gd gd.virtualWidth, y1 = 0 }
                [ ( 0, Color.rgba 0 0 0 1 )
                , ( if v >= 0.95 then
                        0

                    else
                        0.95 - v
                  , Color.rgba 0 0 0 1
                  )
                , ( 1 - v, Color.rgba 0 0 0 0 )
                , ( 1, Color.rgba 0 0 0 0 )
                ]
            , compositeOperationMode DestinationOut
            ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        , shapes
            [ fill col, compositeOperationMode DestinationOver ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        ]
