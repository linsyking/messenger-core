module Scenes.Transition.Scroll exposing (scrollOut)

{-| Scroll Transition

@docs scrollOut

-}

import Canvas exposing (Renderable, group, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (GlobalCompositeOperationMode(..), compositeOperationMode, fillLinear)
import Color exposing (Color)
import Messenger.Coordinate.Coordinates exposing (lengthToReal)
import Messenger.GlobalComponents.Transition.Transitions.Base exposing (SingleTrans)
import Messenger.Render.Shape exposing (rect)


{-| Scroll Out
-}
scrollOut : Renderable -> SingleTrans
scrollOut tord gd rd v =
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
        , group
            [ compositeOperationMode DestinationOver ]
            [ tord
            ]
        ]
