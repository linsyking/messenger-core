module Messenger.UI exposing
    ( Input, Output
    , genMain
    )

{-|


# User Interface

Top-level user interface to the Messenger engine.

@docs Input, Output
@docs genMain

-}

import Audio
import Messenger.Base exposing (Flags, WorldEvent)
import Messenger.Model exposing (Model, audio)
import Messenger.Scene.Scene exposing (AllScenes)
import Messenger.UI.Init exposing (init)
import Messenger.UI.Subscription exposing (subscriptions)
import Messenger.UI.Update exposing (update)
import Messenger.UI.View exposing (view)
import Messenger.UserConfig exposing (UserConfig)


{-| The input to the Messenger UI.
-}
type alias Input userdata scenemsg =
    { config : UserConfig userdata scenemsg
    , allScenes : AllScenes userdata scenemsg
    }


{-| The output of the Messenger UI.
-}
type alias Output userdata scenemsg =
    Program Flags (Audio.Model WorldEvent (Model userdata scenemsg)) (Audio.Msg WorldEvent)


{-| Generate the main program (output) from input.

**Use this for your main function**

-}
genMain : Input userdata scenemsg -> Output userdata scenemsg
genMain input =
    Audio.elementWithAudio
        { init = init input.config input.allScenes
        , update = update input.config input.allScenes
        , subscriptions = subscriptions input.config
        , view = view input.config
        , audio = audio
        , audioPort = { toJS = input.config.ports.audioPortToJS, fromJS = input.config.ports.audioPortFromJS }
        }
