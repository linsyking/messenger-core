module Messenger.Scene.VSR exposing
    ( VSR
    , updateVSR, viewVSR
    )

{-|


# Virtual Scene Runner

@docs VSR
@docs updateVSR, viewVSR

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, UserEvent(..))
import Messenger.Scene.Scene exposing (MAbstractScene, SceneOutputMsg, unroll)


{-| Virtual Scene Runner
-}
type alias VSR userdata scenemsg =
    { env : Env () userdata
    , scene : MAbstractScene userdata scenemsg
    }


{-| Update the VSR.
-}
updateVSR : VSR userdata scenemsg -> UserEvent -> ( VSR userdata scenemsg, List (SceneOutputMsg scenemsg userdata) )
updateVSR vsr evnt =
    let
        env =
            vsr.env

        env1 =
            case evnt of
                Tick delta ->
                    let
                        -- Update the time
                        gd =
                            env.globalData

                        newgd =
                            { gd | sceneStartFrame = gd.sceneStartFrame + 1, sceneStartTime = gd.sceneStartFrame + delta, globalStartTime = gd.sceneStartTime + delta, globalStartFrame = gd.globalStartFrame + 1 }
                    in
                    { env | globalData = newgd }

                _ ->
                    env

        ( newScene, newMsg, newEnv ) =
            (unroll vsr.scene).update env1 evnt
    in
    ( VSR newEnv newScene, newMsg )


{-| View the VSR.
-}
viewVSR : VSR userdata scenemsg -> Renderable
viewVSR vsr =
    (unroll vsr.scene).view vsr.env
