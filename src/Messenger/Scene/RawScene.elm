module Messenger.Scene.RawScene exposing
    ( RawSceneInit, RawSceneUpdate, RawSceneView
    , genRawScene
    , RawSceneProtoInit, RawSceneProtoLevelInit, initCompose
    )

{-|


# RawScene

Raw Scene is a scene without anything. Users can add whatever they like in the raw scene. It is suitable for those who want to imply
their own logic and framework(instead of layer/component) or do a minimum amount of work to achieve simple tasks, like displaying Hello world.

@docs RawSceneInit, RawSceneUpdate, RawSceneView
@docs genRawScene


## Scene Prototype

@docs RawSceneProtoInit, RawSceneProtoLevelInit, initCompose

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg, SceneStorage, abstract)


{-| init type sugar
Initialize the data of the rawscene using environment and scenemsg, and return a SceneData defined by user.
-}
type alias RawSceneInit data userdata scenemsg =
    Env () userdata -> Maybe scenemsg -> data


{-| init type sugar for levels
Here you should describe how to generate the InitData of the scene to be passed to the prototype.
Note in most of the case you may write almost all configs of the scene to be initialized here, like the place of block and the enemies.
However, if you want to pass some parameters at real time inside the game to create new scene, the only pathway is through the scenemsg.
This may happen if you want to load some stored information from local storage or generate the scene based on the user's input.
-}
type alias RawSceneProtoLevelInit userdata scenemsg idata =
    Env () userdata -> Maybe scenemsg -> Maybe idata


{-| init type sugar for scene prototypes
Here you should generate the data using the initial arguments given by InitData.
Receives the environment and the initData and returns the data of the scene.
-}
type alias RawSceneProtoInit data userdata idata =
    Env () userdata -> Maybe idata -> data


{-| update type sugar
The update function, which is similar to that of component and layer(remember the general model)
receives Environment env,UserEvent evt,data of the scene data and returns (data,List SOMMsg) and new env.

  - Note that Scene will only send message to messenger core, so here it will output SOMMsg only.

-}
type alias RawSceneUpdate data userdata scenemsg =
    Env () userdata -> UserEvent -> data -> ( data, List (SceneOutputMsg scenemsg userdata), Env () userdata )


{-| view type sugar
View the raw scene.
Receives the environment(which contains the globalData) and the scene data and return a renderable.
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
