module Messenger.UI exposing
    ( Output
    , genMain
    )

{-|


# User Interface

Top-level user interface to the Messenger engine.

@docs Output
@docs genMain

-}

import Audio
import Messenger.Base exposing (Flags, WorldEvent)
import Messenger.Model exposing (Model)
import Messenger.UI.Init exposing (init)
import Messenger.UI.Input exposing (Input)
import Messenger.UI.Subscription exposing (subscriptions)
import Messenger.UI.Update exposing (update)
import Messenger.UI.View exposing (audio, view)


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
        { init = init input
        , update = update input
        , subscriptions = subscriptions input.config
        , view = view input
        , audio = audio
        , audioPort = { toJS = input.config.ports.audioPortToJS, fromJS = input.config.ports.audioPortFromJS }
        }
