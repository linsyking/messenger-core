module Messenger.Model exposing (..)

{-| Model

This is the main model data.

`currentData` and `currentGlobalData` is writable, `currentScene` is readonly, `time` is readonly.

Those data is **not** exposed to the scene.

We only use it in the main update.

TODO

-}

import Audio exposing (Audio, AudioData)
import Browser.Events exposing (Visibility(..))
import Messenger.Audio.Audio exposing (AudioRepo, getAudio)
import Messenger.Base exposing (GlobalData)
import Messenger.Scene.Scene exposing (MAbstractScene)
import Messenger.Scene.Transitions.Base exposing (Transition)


type alias Model userdata scenemsg =
    { currentScene : MAbstractScene userdata scenemsg
    , currentGlobalData : GlobalData userdata
    , audiorepo : AudioRepo
    , transition : Maybe ( Transition userdata, ( String, Maybe scenemsg ) )
    }


{-| updateSceneTime
-}
updateSceneTime : Model userdata scenemsg -> Model userdata scenemsg
updateSceneTime m =
    let
        ogd =
            m.currentGlobalData

        ngd =
            { ogd | sceneStartTime = ogd.sceneStartTime + 1, globalTime = ogd.globalTime + 1 }
    in
    { m | currentGlobalData = ngd }


{-| resetSceneStartTime
Set the scene starttime to 0.
-}
resetSceneStartTime : Model userdata scenemsg -> Model userdata scenemsg
resetSceneStartTime m =
    let
        ogd =
            m.currentGlobalData

        ngd =
            { ogd | sceneStartTime = 0 }
    in
    { m | currentGlobalData = ngd }


{-| audio

The audio argument needed in the main model.

-}
audio : AudioData -> Model userdata scenemsg -> Audio
audio ad model =
    Audio.group (getAudio ad model.audiorepo)
        |> Audio.scaleVolume model.currentGlobalData.volume
