module Messenger.UI.Input exposing (Input)

{-|


# Input to the Messenger UI

@docs Input

-}

import Messenger.Scene.Scene exposing (AllScenes)
import Messenger.UserConfig exposing (Resources, UserConfig)


{-| The input to the Messenger UI.
-}
type alias Input userdata scenemsg =
    { config : UserConfig userdata scenemsg
    , resources : Resources
    , scenes : AllScenes userdata scenemsg
    }
