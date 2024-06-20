module Scenes.Home2.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas
import Color
import Duration
import GlobalComponents.Transition.Model as Transition
import GlobalComponents.Transition.Transitions.Base exposing (TransitionOption, genTransition, nullTransition)
import GlobalComponents.Transition.Transitions.Fade exposing (fadeInBlack, fadeInTransparent, fadeInWithRenderable, fadeOutBlack, fadeOutTransparent, fadeOutWithRenderable)
import GlobalComponents.Transition.Transitions.Scroll exposing (scrollIn, scrollOut)
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.Render.Sprite exposing (renderSprite)
import Messenger.Render.Text exposing (renderText, renderTextWithColorCenter)
import Messenger.Render.TextBox exposing (renderTextBoxWithColorCenter)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg(..), SceneStorage)
import Messenger.UserConfig exposing (coloredBackground)
import String exposing (fromInt)


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

        KeyDown 52 ->
            ( data
            , [ SOMLoadGC (Transition.genGC (Transition.InitOption (genTransition ( fadeOutBlack, Duration.seconds 1 ) ( fadeInBlack, Duration.seconds 1 ) Nothing) ( "Home", Nothing ) True) Nothing)
              ]
            , env
            )

        KeyDown 53 ->
            ( data
            , [ SOMLoadGC (Transition.genGC (Transition.InitOption (genTransition ( scrollOut Color.black, Duration.seconds 1 ) ( scrollIn Color.black, Duration.seconds 1 ) Nothing) ( "Home", Nothing ) True) Nothing)
              ]
            , env
            )

        _ ->
            ( data, [], env )


view : RawSceneView UserData Data
view env data =
    Canvas.group []
        [ coloredBackground Color.white env.globalData.internalData
        , renderSprite env.globalData.internalData [] ( 0, 300 ) ( 1920, 0 ) "ship"
        , renderTextBoxWithColorCenter env.globalData.internalData 50 "Mode:\n1: Fade out + Fade in, mixed\n2: Fade out transparent + Fade in transparent, sequential\n3: null + Fade in with Renderable, sequential\n4: Fade out black + Fade in black, sequential\n5: Scroll out black + Scroll in black, sequential" "Courier" Color.red ( 1920 / 2, 25 ) ( 1920, 500 )
        , renderText env.globalData.internalData 50 (fromInt env.globalData.sceneStartFrame) "Courier" ( 0, 100 )
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
