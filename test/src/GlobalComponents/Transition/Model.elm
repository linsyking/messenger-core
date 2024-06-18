module GlobalComponents.Transition.Model exposing (InitOption, genGC)

{-|


# Transition

@docs InitOption, genGC

-}

import Canvas
import Dict exposing (Dict)
import GlobalComponents.Transition.Transitions.Base exposing (TransStorage, Transition)
import Json.Encode as E
import Messenger.Scene.Scene exposing (ConcreteGlobalComponent, GCTarget, GlobalComponentInit, GlobalComponentStorage, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView, MAbstractScene, genGlobalComponent)


{-| Options
-}
type alias InitOption =
    Dict String TransStorage


type alias Data userdata scenemsg =
    { userTransitions : InitOption
    , changeSceneTime : Int
    , preScene : Maybe (MAbstractScene userdata scenemsg)
    , transition : Maybe Transition
    }


init : InitOption -> GlobalComponentInit userdata scenemsg (Data userdata scenemsg)
init userTransitions _ _ =
    { userTransitions = userTransitions
    , changeSceneTime = 0
    , preScene = Nothing
    , transition = Nothing
    }


update : GlobalComponentUpdate userdata scenemsg (Data userdata scenemsg)
update env _ data =
    ( data, [], ( env, False ) )


updaterec : GlobalComponentUpdateRec userdata scenemsg (Data userdata scenemsg)
updaterec env _ data =
    ( data, [], env )


view : GlobalComponentView userdata scenemsg (Data userdata scenemsg)
view _ _ =
    Canvas.empty


gcCon : InitOption -> ConcreteGlobalComponent (Data userdata scenemsg) userdata scenemsg
gcCon opt =
    { init = init opt
    , update = update
    , updaterec = updaterec
    , view = view
    , matcher = "fps"
    }


{-| Generate a global component.
-}
genGC : InitOption -> Maybe GCTarget -> GlobalComponentStorage userdata scenemsg
genGC opt =
    genGlobalComponent (gcCon opt) E.null
