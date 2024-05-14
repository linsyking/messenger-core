module Messenger.Scene.RawScene exposing
    ( RawSceneInit, RawSceneUpdate, RawSceneView
    , genRawScene
    , RawSceneProtoInit, RawSceneProtoLevelInit, initCompose
    )

{-|


# RawScene

Raw Scene is a scene without anything. Users can add whatever they like in the raw scene!

@docs RawSceneInit, RawSceneUpdate, RawSceneView
@docs genRawScene


## Scene Prototype

@docs RawSceneProtoInit, RawSceneProtoLevelInit, initCompose

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg, SceneStorage, abstract)


{-| init type sugar
-}
type alias RawSceneInit data userdata scenemsg =
    Env () userdata -> Maybe scenemsg -> data


{-| init type sugar for levels
-}
type alias RawSceneProtoLevelInit userdata scenemsg idata =
    Env () userdata -> Maybe scenemsg -> Maybe idata


{-| init type sugar for scene prototypes
-}
type alias RawSceneProtoInit data userdata idata =
    Env () userdata -> Maybe idata -> data


{-| update type sugar
-}
type alias RawSceneUpdate data userdata scenemsg =
    Env () userdata -> UserEvent -> data -> ( data, List (SceneOutputMsg scenemsg userdata), Env () userdata )


{-| view type sugar
-}
type alias RawSceneView userdata data =
    Env () userdata -> data -> Renderable


{-| Generate a raw scene from a concrete scene.
-}
genRawScene : MConcreteScene data userdata scenemsg -> SceneStorage userdata scenemsg
genRawScene =
    abstract


{-| Compose RawSceneProtoInit with RawSceneProtoLevelInit.
-}
initCompose : RawSceneProtoInit data userdata idata -> RawSceneProtoLevelInit userdata scenemsg idata -> RawSceneInit data userdata scenemsg
initCompose pinit linit env msg =
    pinit env <| linit env msg
