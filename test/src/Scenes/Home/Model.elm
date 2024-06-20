module Scenes.Home.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas
import Color
import Duration
import GlobalComponents.Transition.Model as Transition
import GlobalComponents.Transition.Transitions.Base exposing (genTransition, nullTransition)
import GlobalComponents.Transition.Transitions.Fade exposing (fadeInBlack, fadeInTransparent, fadeInWithRenderable, fadeOutBlack, fadeOutTransparent)
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.Render.TextBox exposing (renderTextBox)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg(..), SceneStorage)
import Messenger.UserConfig exposing (coloredBackground)


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
            , [ SOMLoadGC (Transition.genGC (Transition.InitOption (genTransition ( fadeOutTransparent, Duration.seconds 1 ) ( fadeInTransparent, Duration.seconds 1 ) Nothing) ( "Home2", Nothing ) True) Nothing)
              ]
            , env
            )

        KeyDown 50 ->
            ( data
            , [ SOMChangeScene Nothing "Stress"
              ]
            , env
            )

        _ ->
            ( data, [], env )


view : RawSceneView UserData Data
view env data =
    Canvas.group []
        [ coloredBackground Color.yellow env.globalData.internalData
        , renderTextBox env.globalData.internalData 50 "Menu\n1. Test Transition\n2. Rendering Stress Test" "Courier" ( 0, 0 ) ( 1920, 1080 )
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
