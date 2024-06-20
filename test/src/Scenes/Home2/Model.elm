module Scenes.Home2.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas
import Color
import Duration
import GlobalComponents.Transition.Model as Transition
import GlobalComponents.Transition.Transitions.Base exposing (TransitionOption, genTransition, nullTransition)
import GlobalComponents.Transition.Transitions.Fade exposing (fadeInTransparent, fadeInWithRenderable, fadeOutTransparent, fadeOutWithRenderable)
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.Render.Sprite exposing (renderSprite)
import Messenger.Render.TextBox exposing (renderTextBoxWithColorCenter)
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
            , [ SOMLoadGC (Transition.genGC (Transition.InitOption (genTransition ( fadeOutTransparent, Duration.seconds 1 ) ( fadeInTransparent, Duration.seconds 1 ) (Just <| TransitionOption True)) ( "Home", Nothing ) True) Nothing)
              ]
            , env
            )

        KeyDown 50 ->
            ( data
            , [ SOMLoadGC (Transition.genGC (Transition.InitOption (genTransition ( fadeOutTransparent, Duration.seconds 1 ) ( fadeInTransparent, Duration.seconds 1 ) Nothing) ( "Home", Nothing ) True) Nothing)
              ]
            , env
            )

        KeyDown 51 ->
            ( data
            , [ SOMLoadGC (Transition.genGC (Transition.InitOption (genTransition ( nullTransition, Duration.seconds 0 ) ( fadeInWithRenderable <| view env data, Duration.seconds 1 ) Nothing) ( "Home", Nothing ) True) Nothing)
              ]
            , env
            )

        _ ->
            ( data, [], env )


view : RawSceneView UserData Data
view env data =
    Canvas.group []
        [ renderSprite env.globalData.internalData [] ( 0, 0 ) ( 1920, 0 ) "ship"
        , renderTextBoxWithColorCenter env.globalData.internalData 50 "HELLO\nWORLD!" "Courier" Color.blue ( 1920 / 2, 1080 / 2 )
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
