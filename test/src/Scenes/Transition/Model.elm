module Scenes.Transition.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Color
import Duration
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.GlobalComponents.Transition.Base exposing (nullTransition)
import Messenger.GlobalComponents.Transition.Model exposing (genMixedTransitionSOM, genSequentialTransitionSOM)
import Messenger.GlobalComponents.Transition.Transitions exposing (fadeIn, fadeInWithRenderable, fadeMix, fadeOut)
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg(..), SceneStorage)
import Quantity
import REGL
import REGL.BuiltinPrograms as P
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
            , [ genMixedTransitionSOM ( fadeMix, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 50 ->
            ( data
            , [ genSequentialTransitionSOM ( fadeOut, Duration.seconds 1 ) ( fadeIn, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        KeyDown 51 ->
            ( data
            , [ genSequentialTransitionSOM ( nullTransition, Quantity.zero ) ( fadeInWithRenderable <| view env data, Duration.seconds 1 ) ( "Home", Nothing )
              ]
            , env
            )

        _ ->
            ( data, [], env )


comment : String
comment =
    """Mode:
1: Fade out + Fade in, mixed
2: Fade out black + Fade in black, sequential
3: null + Fade in with Renderable, sequential
"""


view : RawSceneView UserData Data
view env data =
    REGL.group []
        [ P.clear Color.lightGreen
        , P.textbox ( 0, 1080 ) 40 comment "arial" Color.black
        , P.textbox ( 0, 100 ) 30 (fromInt env.globalData.sceneStartFrame) "arial" Color.black
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
