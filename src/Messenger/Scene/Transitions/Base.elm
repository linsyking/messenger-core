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
type alias SingleTrans ls =
    GlobalData ls -> Renderable -> Float -> Renderable


{-| Null Transition
-}
nullTransition : SingleTrans ls
nullTransition _ r _ =
    r


{-| Transition has three stages:

1.  From the old scene to the transition scene
2.  Transition scene
3.  From the transition scene to the new scene

-}
type alias Transition ls =
    { currentTransition : Int
    , outT : Int
    , inT : Int
    , fadeout : SingleTrans ls
    , fadein : SingleTrans ls
    }


{-| Generate new transition
-}
genTransition : Int -> Int -> SingleTrans ls -> SingleTrans ls -> Transition ls
genTransition outT inT fadeout fadein =
    { currentTransition = 0
    , outT = outT
    , inT = inT
    , fadeout = fadeout
    , fadein = fadein
    }
