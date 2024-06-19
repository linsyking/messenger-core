module Scenes.Home2.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas
import Color
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Render.Sprite exposing (renderSprite)
import Messenger.Render.TextBox exposing (renderTextBoxWithColorCenter)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneStorage)


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
    Canvas.group []
        [ renderSprite env.globalData.internalData [] ( 0, 0 ) ( 1920, 0 ) "ship"
        , renderTextBoxWithColorCenter env.globalData.internalData 50 "Menu\n1. Test Rendering\n2. Test Layer\n3. Test Component\n4. Test Transition" "Courier" Color.blue ( 1920 / 2, 1080 / 2 )
        ]


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
