module Scenes.Stress.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Color
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneStorage)
import REGL


type alias Data =
    {}


init : RawSceneInit Data UserData SceneMsg
init env msg =
    {}


update : RawSceneUpdate Data UserData SceneMsg
update env msg data =
    ( data, [], env )


view : RawSceneView UserData Data
view env data =
    let
        time =
            env.globalData.sceneStartFrame
    in
    REGL.group [] <|
        REGL.clear Color.white
            :: (List.concat <|
                    List.map
                        (\x ->
                            List.map
                                (\y ->
                                    REGL.centeredTexture ( toFloat x * 20 + toFloat time, toFloat y * 20 + 20 ) ( 20, 20 ) 0 "ship"
                                )
                                (List.range 0 50)
                        )
                        (List.range 0 100)
               )


scenecon : MConcreteScene Data UserData SceneMsg
scenecon =
    { init = init
    , update = update
    , view = view
    }


{-| Scene generator
-}
scene : SceneStorage UserData SceneMsg
scene =
    genRawScene scenecon
