module Messenger.Model exposing
    ( Model
    , updateSceneTime, resetSceneStartTime
    )

{-|


# Model

This is the main model data.

Those data is **not** exposed to the scene.

We only use it in the main update.

@docs Model
@docs updateSceneTime, resetSceneStartTime

-}

import Browser.Events exposing (Visibility(..))
import Messenger.Base exposing (GlobalData)
import Messenger.Scene.Scene exposing (MAbstractScene)
import Messenger.Scene.Transitions.Base exposing (Transition)


{-| The model for the game
-}
type alias Model userdata scenemsg =
    { currentScene : MAbstractScene userdata scenemsg
    , currentGlobalData : GlobalData userdata
    , transition : Maybe ( Transition userdata, ( String, Maybe scenemsg ) )
    }


{-| Update scene start time and global time
-}
updateSceneTime : Model userdata scenemsg -> Model userdata scenemsg
updateSceneTime m =
    let
        gd =
            m.currentGlobalData

        ngd =
            { gd | sceneStartTime = gd.sceneStartTime + 1 }
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
