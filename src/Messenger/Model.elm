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
import Canvas exposing (Renderable)
import Messenger.Base exposing (Env)
import Messenger.Scene.Scene exposing (AbstractGlobalComponent, MAbstractScene)


{-| The model for the game
-}
type alias Model userdata scenemsg =
    { env : Env (MAbstractScene userdata scenemsg) userdata
    , globalComponents : List (AbstractGlobalComponent userdata scenemsg)
    , canvasRenderable : List Renderable
    }


{-| Update scene start time and global time
-}
updateSceneTime : Model userdata scenemsg -> Int -> Model userdata scenemsg
updateSceneTime m delta =
    let
        gd =
            env.globalData

        env =
            m.env

        ngd =
            { gd | sceneStartTime = gd.sceneStartTime + delta, sceneStartFrame = gd.sceneStartFrame + 1 }
    in
    { m | env = { env | globalData = ngd } }


{-| Reset the scene starttime to 0.
-}
resetSceneStartTime : Model userdata scenemsg -> Model userdata scenemsg
resetSceneStartTime m =
    let
        gd =
            env.globalData

        env =
            m.env

        ngd =
            { gd | sceneStartTime = 0, sceneStartFrame = 0 }
    in
    { m | env = { env | globalData = ngd } }
