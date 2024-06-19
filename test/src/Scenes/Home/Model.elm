module Scenes.Home.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.Render.TextBox exposing (renderTextBox)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg(..), SceneStorage)


type alias Data =
    {}


init : RawSceneInit Data UserData SceneMsg
init env msg =
    {}


update : RawSceneUpdate Data UserData SceneMsg
update env msg data =
    case msg of
        KeyDown 49 ->
            ( data, [ SOMChangeScene Nothing "Home2", SOMUnloadGC "fps" ], env )

        _ ->
            ( data, [], env )


view : RawSceneView UserData Data
view env data =
    Canvas.group []
        [ renderTextBox env.globalData.internalData 50 "Menu\n1. Test Rendering\n2. Test Layer\n3. Test Component\n4. Test Transition" "Courier" ( 0, 0 )
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
