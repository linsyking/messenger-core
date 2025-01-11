module Scenes.Home.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Color
import Duration
import Lib.Base exposing (SceneMsg)
import Lib.Resources exposing (resources)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..), loadedResourceNum)
import Messenger.GlobalComponents.Transition.Model exposing (genSequentialTransitionSOM)
import Messenger.GlobalComponents.Transition.Transitions exposing (fadeIn, fadeOut)
import Messenger.Render.Texture exposing (renderSprite)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg(..), SceneStorage)
import Messenger.UserConfig exposing (resourceNum)
import REGL
import REGL.BuiltinPrograms as P


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
            , [ genSequentialTransitionSOM ( fadeOut, Duration.seconds 1 ) ( fadeIn, Duration.seconds 1 ) ( "Transition", Nothing )
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
    REGL.group []
        [ P.clear Color.lightYellow
        , P.textbox ( 0, 30 ) 50 "Menu\n1. Transition Test\n2. Rendering Stress Test\n3. Audio Test" "firacode" Color.black
        , renderSprite env.globalData.internalData ( 100, 200 ) ( 0, 200 ) "ship"
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
