module GlobalComponents.Transition.Model exposing (..)

import Dict exposing (Dict)
import GlobalComponents.Transition.Transitions.Base exposing (Transition)


type alias Data =
    { transitions : Dict String Transition
    }
