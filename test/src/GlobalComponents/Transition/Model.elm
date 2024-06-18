module GlobalComponents.Transition.Model exposing (..)

import Dict exposing (Dict)
import GlobalComponents.Transition.Transitions.Base exposing (Transition)
import Messenger.Scene.Scene exposing (MAbstractScene)


type alias Data userdata scenemsg =
    { transitions : Dict String Transition
    , changeSceneTime : Int
    , preScene : Maybe (MAbstractScene userdata scenemsg)
    , transition : Maybe Transition
    }
