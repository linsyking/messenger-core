module GlobalComponents.Transition.Transitions.Fade exposing
    ( fadeIn, fadeOut
    , fadeOutBlack, fadeInBlack
    , fadeOutWithRenderable, fadeInWithRenderable
    )

{-| Fading Effects

@docs fadeIn, fadeOut
@docs fadeOutBlack, fadeInBlack
@docs fadeOutWithRenderable, fadeInWithRenderable

-}

import Canvas exposing (Renderable, group, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (alpha)
import Color exposing (Color)
import GlobalComponents.Transition.Transitions.Base exposing (SingleTrans)
import Messenger.Render.Shape exposing (rect)


{-| Fade Out with Color
-}
fadeOut : Color -> SingleTrans
fadeOut color gd rd v =
    group []
        [ rd
        , shapes [ fill color, alpha v ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        ]


{-| Fade In with Color
-}
fadeIn : Color -> SingleTrans
fadeIn color gd rd v =
    group []
        [ rd
        , shapes [ fill color, alpha (1 - v) ]
            [ rect gd ( 0, 0 ) ( gd.virtualWidth, gd.virtualHeight )
            ]
        ]


{-| Fade Out with Black
-}
fadeOutBlack : SingleTrans
fadeOutBlack =
    fadeOut Color.black


{-| Fade In with Black
-}
fadeInBlack : SingleTrans
fadeInBlack =
    fadeIn Color.black


{-| Fade Out with Renderable
-}
fadeOutWithRenderable : Renderable -> SingleTrans
fadeOutWithRenderable renderable _ rd v =
    group []
        [ rd
        , group [ alpha v ]
            [ renderable
            ]
        ]


{-| Fade In with Renderable
-}
fadeInWithRenderable : Renderable -> SingleTrans
fadeInWithRenderable renderable _ rd v =
    group []
        [ rd
        , group [ alpha (1 - v) ]
            [ renderable
            ]
        ]
