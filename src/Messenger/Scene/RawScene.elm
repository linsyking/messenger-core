module Messenger.Scene.RawScene exposing
    ( RawSceneInit, RawSceneUpdate, RawSceneView
    , genRawScene
    )

{-|


# RawScene

Raw Scene is a scene without anything. Users can add whatever they like in the raw scene!

@docs RawSceneInit, RawSceneUpdate, RawSceneView
@docs genRawScene

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, WorldEvent)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg, SceneStorage, abstract)


{-| init type sugar
-}
type alias RawSceneInit data userdata scenemsg =
    Env () userdata -> Maybe scenemsg -> data


{-| update type sugar
-}
type alias RawSceneUpdate data userdata scenemsg =
    Env () userdata -> WorldEvent -> data -> ( data, List (SceneOutputMsg scenemsg userdata), Env () userdata )


{-| view type sugar
-}
type alias RawSceneView userdata data =
    Env () userdata -> data -> Renderable


{-| generate a raw scene from a concrete scene
-}
genRawScene : MConcreteScene data userdata scenemsg -> SceneStorage userdata scenemsg
genRawScene =
    abstract
