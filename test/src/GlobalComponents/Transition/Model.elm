module GlobalComponents.Transition.Model exposing (..)

import Dict exposing (Dict)
import GlobalComponents.Transition.Transitions.Base exposing (TransStorage, Transition)
import Messenger.Scene.Scene exposing (GlobalComponentInit, MAbstractScene)


type alias Data userdata scenemsg =
    { userTransitions : Dict String TransStorage
    , changeSceneTime : Int
    , preScene : Maybe (MAbstractScene userdata scenemsg)
    , transition : Maybe Transition
    }


init : Dict String TransStorage -> GlobalComponentInit userdata scenemsg (Data userdata scenemsg)
init userTransitions env msg =
    { userTransitions = userTransitions
    , changeSceneTime = 0
    , preScene = Nothing
    , transition = Nothing
    }
