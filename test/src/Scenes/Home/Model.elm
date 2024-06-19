module Scenes.Home.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas
import Duration
import GlobalComponents.FPS.Model as FPS
import GlobalComponents.Transition.Model as Transition
import GlobalComponents.Transition.Transitions.Base exposing (genTransition)
import GlobalComponents.Transition.Transitions.Fade exposing (fadeInBlack, fadeOutBlack)
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
            ( data
            , [ 
            --     SOMUnloadGC "fps"
            --   , SOMLoadGC (FPS.genGC (FPS.InitOption 100) Nothing)
            --   , 
              SOMLoadGC (Transition.genGC ( genTransition ( fadeOutBlack, Duration.seconds 1 ) ( fadeInBlack, Duration.seconds 1 ) Nothing, "Home2", Nothing ) Nothing)
              ]
            , env
            )

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
