module Messenger.UI exposing (..)

import Audio
import Messenger.Audio.Audio exposing (audioPortFromJS, audioPortToJS)
import Messenger.Base exposing (Flags, WorldEvent)
import Messenger.Model exposing (Model, audio)
import Messenger.Scene.Loader exposing (SceneStorage)
import Messenger.UI.Init exposing (init)
import Messenger.UI.Subscription exposing (subscriptions)
import Messenger.UI.Update exposing (update)
import Messenger.UI.View exposing (view)
import Messenger.UserConfig exposing (UserConfig)


type alias Scenes userdata scenemsg =
    List ( String, SceneStorage userdata scenemsg )


type alias Input userdata scenemsg =
    { config : UserConfig userdata scenemsg
    , allScenes : Scenes userdata scenemsg
    }


type alias Output userdata scenemsg =
    Program Flags (Audio.Model WorldEvent (Model userdata scenemsg)) (Audio.Msg WorldEvent)


genMain : Input userdata scenemsg -> Output userdata scenemsg
genMain input =
    Audio.elementWithAudio
        { init = init input.config input.allScenes
        , update = update input.config input.allScenes
        , subscriptions = subscriptions input.config
        , view = view input.config
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }
