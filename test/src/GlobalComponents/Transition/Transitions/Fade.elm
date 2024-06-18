module GlobalComponents.Transition.Transitions.Fade exposing
    ( fadeIn, fadeInStorage
    , fadeOut, fadeOutStorage
    , fadeOutBlack, fadeInBlack
    )

{-| Fading Effects

@docs fadeIn, fadeInStorage
@docs fadeOut, fadeOutStorage
@docs fadeOutBlack, fadeInBlack

-}

import Canvas exposing (group, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (alpha)
import Color exposing (Color)
import GlobalComponents.Transition.Transitions.Base exposing (SingleTrans, TransStorage, colorDec, colorEnc)
import Messenger.Render.Shape exposing (rect)
import Messenger.Scene.Scene exposing (GCMsg)


{-| Fade Out with Color
-}
fadeOut : Color -> ( String, GCMsg )
fadeOut col =
    ( "FadeOut", colorEnc col )


{-| FadeOut Storage
-}
fadeOutStorage : TransStorage
fadeOutStorage msg =
    fadeOutHelper (colorDec msg)


fadeOutHelper : Color -> SingleTrans
fadeOutHelper color gd rd v =
    group []
        [ rd
        , shapes [ fill color, alpha v ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        ]


{-| Fade In with Color
-}
fadeIn : Color -> ( String, GCMsg )
fadeIn col =
    ( "FadeIn", colorEnc col )


{-| FadeIn Storage
-}
fadeInStorage : TransStorage
fadeInStorage msg =
    fadeInHelper (colorDec msg)


fadeInHelper : Color -> SingleTrans
fadeInHelper color gd rd v =
    group []
        [ rd
        , shapes [ fill color, alpha (1 - v) ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        ]


{-| Fade Out with Black
-}
fadeOutBlack : ( String, GCMsg )
fadeOutBlack =
    fadeOut Color.black


{-| Fade In with Black
-}
fadeInBlack : ( String, GCMsg )
fadeInBlack =
    fadeIn Color.black
