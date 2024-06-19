module GlobalComponents.Transition.Transitions.Base exposing
    ( Transition, SingleTrans, TransitionOption
    , genTransition, nullTransition
    )

{-|


# Transition Base

@docs Transition, SingleTrans, TransitionOption
@docs genTransition, nullTransition

-}

import Canvas exposing (Renderable)
import Duration exposing (Duration)
import Messenger.Base exposing (InternalData)


{-| Single Transition
-}
type alias SingleTrans =
    InternalData -> Renderable -> Float -> Renderable


{-| Null Transition
-}
nullTransition : SingleTrans
nullTransition _ r _ =
    r


{-| Transition has two stages:

1.  From the old scene to the transition scene
2.  From the transition scene to the new scene

-}
type alias Transition =
    { currentTransition : Int
    , outT : Int
    , inT : Int
    , outTrans : SingleTrans
    , inTrans : SingleTrans
    , options : TransitionOption
    }


{-| Transition option
-}
type alias TransitionOption =
    { mix : Bool
    }


defaultOption : TransitionOption
defaultOption =
    { mix = False
    }


{-| Generate new transition
-}
genTransition : ( SingleTrans, Duration ) -> ( SingleTrans, Duration ) -> Maybe TransitionOption -> Transition
genTransition ( outTrans, outT ) ( inTrans, inT ) opts =
    { currentTransition = 0
    , outT = ceiling <| Duration.inMilliseconds outT
    , inT = ceiling <| Duration.inMilliseconds inT
    , outTrans = outTrans
    , inTrans = inTrans
    , options = Maybe.withDefault defaultOption opts
    }
