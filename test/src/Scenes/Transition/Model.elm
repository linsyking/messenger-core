module Scenes.Transition.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Canvas exposing (group, shapes)
import Canvas.Settings exposing (fill)
import Color
import Duration
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.GlobalComponents.Transition.Model exposing (genMixedTransitionSOM, genSequentialTransitionSOM)
import Messenger.GlobalComponents.Transition.Transitions.Base exposing (nullTransition)
import Messenger.GlobalComponents.Transition.Transitions.Fade exposing (fadeInBlack, fadeInTransparent, fadeInWithRenderable, fadeOutBlack, fadeOutTransparent, fadeOutWithRenderable)
import Messenger.GlobalComponents.Transition.Transitions.Scroll exposing (scrollIn, scrollInWithRenderable, scrollOut, scrollOutWithRenderable)
import Messenger.Render.Shape exposing (circle, rect)
import Messenger.Render.Sprite exposing (renderSprite)
import Messenger.Render.Text exposing (renderText)
import Messenger.Render.TextBox exposing (renderTextBoxWithColor)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg(..), SceneStorage)
import Messenger.UserConfig exposing (coloredBackground)
import Quantity
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
            , [ genMixedTransitionSOM ( fadeOutTransparent, Duration.seconds 1 ) ( fadeInTransparent, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 50 ->
            ( data
            , [ genSequentialTransitionSOM ( fadeOutTransparent, Duration.seconds 1 ) ( fadeInTransparent, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 51 ->
            ( data
            , [ genSequentialTransitionSOM ( nullTransition, Quantity.zero ) ( fadeInWithRenderable <| view env data, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 52 ->
            ( data
            , [ genSequentialTransitionSOM ( fadeOutBlack, Duration.seconds 1 ) ( fadeInBlack, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 53 ->
            ( data
            , [ genSequentialTransitionSOM ( scrollOut Color.black, Duration.seconds 1 ) ( scrollIn Color.black, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 54 ->
            ( data
            , [ genSequentialTransitionSOM ( scrollOutWithRenderable <| transitionTo env data, Duration.seconds 1 ) ( scrollInWithRenderable <| transitionTo env data, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 55 ->
            ( data
            , [ genSequentialTransitionSOM ( nullTransition, Quantity.zero ) ( scrollInWithRenderable <| view env data, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        _ ->
            ( data, [], env )


transitionTo : RawSceneView UserData Data
transitionTo env _ =
    group []
        [ coloredBackground Color.blue env.globalData.internalData
        , shapes [ fill Color.red ]
            [ rect env.globalData.internalData ( 100, 300 ) ( 500, 200 )
            , circle env.globalData.internalData ( 150, 500 ) 200
            ]
        , renderSprite env.globalData.internalData [] ( 200, 0 ) ( 1920, 0 ) "ship"
        ]


comment : String
comment =
    """Mode:
1: Fade out + Fade in, mixed
2: Fade out transparent + Fade in transparent, sequential
3: null + Fade in with Renderable, sequential
4: Fade out black + Fade in black, sequential
5: Scroll out black + Scroll in black, sequential
6: Scroll out to renderable + Scroll in to renderable, sequential
7: null + Scroll in with renderable, sequential
"""


view : RawSceneView UserData Data
view env data =
    Canvas.group []
        [ coloredBackground Color.white env.globalData.internalData
        , renderSprite env.globalData.internalData [] ( 0, 300 ) ( 1920, 0 ) "ship"
        , renderTextBoxWithColor env.globalData.internalData 50 comment "Courier" Color.red ( 0, 0 ) ( 1920, 500 )
        , renderText env.globalData.internalData 50 (fromInt env.globalData.sceneStartFrame) "Courier" ( 0, 900 )
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
