module Messenger.UI.Input exposing (Input)

{-|


# Input to the Messenger UI

@docs Input

-}

import Messenger.Resources.Base exposing (ResourceDefs)
import Messenger.Scene.Scene exposing (AllScenes, GlobalComponentStorage)
import Messenger.UserConfig exposing (UserConfig)


{-| The input to the Messenger UI.
-}
type alias Input userdata scenemsg =
    { config : UserConfig userdata scenemsg
    , resources : ResourceDefs
    , scenes : AllScenes userdata scenemsg
    , globalComponents : List (GlobalComponentStorage userdata scenemsg)
    }
