module GlobalComponents.Transition.Model exposing (..)

import Dict exposing (Dict)
import GlobalComponents.Transition.Transitions.Base exposing (SingleTrans, Transition)
import Messenger.Scene.Scene exposing (GCMsg, MAbstractScene)


type alias Data userdata scenemsg =
    { userTransitions : Dict String (GCMsg -> SingleTrans)
    , changeSceneTime : Int
    , preScene : Maybe (MAbstractScene userdata scenemsg)
    , transition : Maybe Transition
    }

