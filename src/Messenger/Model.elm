module Messenger.Model exposing
    ( Model
    , updateSceneTime, resetSceneStartTime
    , audio
    )

{-|


# Model

This is the main model data.

Those data is **not** exposed to the scene.

We only use it in the main update.

@docs Model
@docs updateSceneTime, resetSceneStartTime
@docs audio

-}

import Audio exposing (Audio, AudioData)
import Browser.Events exposing (Visibility(..))
import Messenger.Audio.Audio exposing (AudioRepo, getAudio)
import Messenger.Base exposing (GlobalData)
import Messenger.Scene.Scene exposing (MAbstractScene)
import Messenger.Scene.Transitions.Base exposing (Transition)


{-| The model for the game
-}
type alias Model userdata scenemsg =
    { currentScene : MAbstractScene userdata scenemsg
    , currentGlobalData : GlobalData userdata
    , audiorepo : AudioRepo
    , transition : Maybe ( Transition userdata, ( String, Maybe scenemsg ) )
    }


{-| Update scene start time and global time
-}
updateSceneTime : Model userdata scenemsg -> Model userdata scenemsg
updateSceneTime m =
    let
        ogd =
            m.currentGlobalData

        ngd =
            { ogd | sceneStartTime = ogd.sceneStartTime + 1 }
    in
    { m | currentGlobalData = ngd }


{-| Reset the scene starttime to 0.
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


{-| Audio view function

The audio argument needed in the main model.

-}
audio : AudioData -> Model userdata scenemsg -> Audio
audio ad model =
    Audio.group (getAudio ad model.audiorepo)
        |> Audio.scaleVolume model.currentGlobalData.volume
