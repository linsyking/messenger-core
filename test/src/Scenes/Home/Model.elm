module Scenes.Home.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas
import Color
import Duration
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.GlobalComponents.Transition.Model exposing (genSequentialTransitionSOM)
import Messenger.GlobalComponents.Transition.Transitions.Fade exposing (fadeInTransparent, fadeOutTransparent)
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
            , [ genSequentialTransitionSOM ( fadeOutTransparent, Duration.seconds 1 ) ( fadeInTransparent, Duration.seconds 1 ) ( "Transition", Nothing )
              ]
            , env
            )

        KeyDown 50 ->
            ( data
            , [ SOMChangeScene Nothing "Stress"
              ]
            , env
            )

        KeyDown 51 ->
            ( data
            , [ SOMChangeScene Nothing "Audio"
              ]
            , env
            )

        _ ->
            ( data, [], env )


view : RawSceneView UserData Data
view env data =
    Canvas.group []
        [ coloredBackground Color.yellow env.globalData.internalData
        , renderTextBox env.globalData.internalData 50 "Menu\n1. Transition Test\n2. Rendering Stress Test\n3. Audio Test" "Courier" ( 0, 0 ) ( 1920, 1080 )
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
