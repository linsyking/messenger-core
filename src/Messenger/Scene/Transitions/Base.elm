module Messenger.Scene.Transitions.Base exposing
    ( Transition, SingleTrans
    , genTransition, nullTransition
    )

{-|


# Transition Base

@docs Transition, SingleTrans
@docs genTransition, nullTransition

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (GlobalData)


{-| Single Transition
-}
type alias SingleTrans userdata =
    GlobalData userdata -> Renderable -> Float -> Renderable


{-| Null Transition
-}
nullTransition : SingleTrans userdata
nullTransition _ r _ =
    r


{-| Transition has two stages:

1.  From the old scene to the transition scene
2.  From the transition scene to the new scene

-}
type alias Transition userdata =
    { currentTransition : Int
    , outT : Int
    , inT : Int
    , fadeout : SingleTrans userdata
    , fadein : SingleTrans userdata
    }


{-| Generate new transition
-}
genTransition : Int -> Int -> SingleTrans userdata -> SingleTrans userdata -> Transition userdata
genTransition outT inT fadeout fadein =
    { currentTransition = 0
    , outT = outT
    , inT = inT
    , fadeout = fadeout
    , fadein = fadein
    }
