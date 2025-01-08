module Messenger.Scene.LayeredScene exposing
    ( LayeredSceneData
    , genLayeredScene
    , LayeredSceneInit, LayeredSceneEffectFunc
    , LayeredSceneProtoInit, LayeredSceneLevelInit, initCompose
    )

{-|


# Layered Scene

Layered scene is a pre-defined scene implementation provided by Messenger.
A layered scene can only handle a list of layers with fixed `cdata`, `userdata`, `tar`, `msg` and `scenemsg` types.

@docs LayeredSceneData
@docs genLayeredScene
@docs LayeredSceneInit, LayeredSceneEffectFunc


## Scene Prototype

@docs LayeredSceneProtoInit, LayeredSceneLevelInit, initCompose

-}

import Messenger.Base exposing (Env, UserEvent, addCommonData, removeCommonData)
import Messenger.GeneralModel exposing (MsgBase(..), filterSOM, viewModelList)
import Messenger.Layer.Layer exposing (AbstractLayer)
import Messenger.Recursion exposing (updateObjects)
import Messenger.Scene.Scene exposing (SceneOutputMsg, SceneStorage, abstract)
import REGL exposing (Effect, Renderable, group)


{-| Layered Scene Data.

    - `renderSettings` is used in `group` while viewing the layers as a whole
    - `commonData` is the common data for the whole scene
    - `layers` are the layers of the scene with type `AbstractLayer`  DO REMEMBER TO ADD YOUR LAYERS HERE!

Here is an example for it:

    { renderSettings = []
    , commonData = cd
    , layers =
        [ Main.layer (MainInitData { components = comps, fakeBricks = fakes, isBoss = isBoss }) envcd
        , TechTree.layer NullLayerMsg envcd
        , TextLayer.layer (TextInitData levelName) envcd
        , PauseLayer.layer NullLayerMsg envcd
        , Guide.layer NullLayerMsg envcd
        ]
    }

-}
type alias LayeredSceneData cdata userdata tar msg scenemsg =
    { renderSettings : List Effect
    , commonData : cdata
    , layers : List (AbstractLayer cdata userdata tar msg scenemsg)
    }


updateLayeredScene : (Env () userdata -> UserEvent -> LayeredSceneData cdata userdata tar msg scenemsg -> List Effect) -> Env () userdata -> UserEvent -> LayeredSceneData cdata userdata tar msg scenemsg -> ( LayeredSceneData cdata userdata tar msg scenemsg, List (SceneOutputMsg scenemsg userdata), Env () userdata )
updateLayeredScene settingsFunc env evt lsd =
    let
        ( newLayers, newMsgs, ( newEnv, _ ) ) =
            updateObjects (addCommonData lsd.commonData env) evt lsd.layers

        som =
            filterSOM newMsgs
    in
    ( { renderSettings = settingsFunc env evt lsd, commonData = newEnv.commonData, layers = newLayers }, som, removeCommonData newEnv )


viewLayeredScene : Env () userdata -> LayeredSceneData cdata userdata tar msg scenemsg -> Renderable
viewLayeredScene env { renderSettings, commonData, layers } =
    viewModelList (addCommonData commonData env) layers
        |> group renderSettings


{-| init type sugar for normal (not prototype) layered scenes
Receives Environment and sceneMsg (which stores the initial data), and return a record of data, for example this:

       { renderSettings = []
        , commonData = cd
        , layers =
            [ Opening.layer NullLayerMsg envcd ]
        }

  - Note: The most important part here is the initialization of scenecommondata. This is so important that it usually takes a separate function to do it.

-}
type alias LayeredSceneInit cdata userdata tar msg scenemsg =
    Env () userdata -> Maybe scenemsg -> LayeredSceneData cdata userdata tar msg scenemsg


{-| init type sugar for levels.
Normally it is used internally. No need to worry about it.
-}
type alias LayeredSceneLevelInit userdata scenemsg idata =
    Env () userdata -> Maybe scenemsg -> Maybe idata


{-| init type sugar for scene prototypes
This defines the initializing function for a scene prototype. It should be like this:

LayeredSceneProtoInit SceneCommonData UserData LayerTarget (LayerMsg SceneMsg) SceneMsg (InitData SceneMsg)

It receives environment, the initial data to initialize the prototype, and return a layeredSceneData.

-}
type alias LayeredSceneProtoInit cdata userdata tar msg scenemsg idata =
    Env () userdata -> Maybe idata -> LayeredSceneData cdata userdata tar msg scenemsg


{-| settingsFunc type sugar
This function takes the environment, userevent, the data of the scene and add visual effects to the scene.
-}
type alias LayeredSceneEffectFunc cdata userdata tar msg scenemsg =
    Env () userdata -> UserEvent -> LayeredSceneData cdata userdata tar msg scenemsg -> List Effect


{-| This creates a layered scene.

  - `init` creates the initial layered scene from env data and init msg.

  - `settingsFunc` is a user provided function to modify `renderSettings` each time the scene updates. If you don't need `settingsFunc`, simply provide a `func _ _ _ = settings`

-}
genLayeredScene : LayeredSceneInit cdata userdata tar msg scenemsg -> LayeredSceneEffectFunc cdata userdata tar msg scenemsg -> SceneStorage userdata scenemsg
genLayeredScene init settingsFunc =
    abstract
        { init = init
        , update = updateLayeredScene settingsFunc
        , view = viewLayeredScene
        }


{-| Compose LayeredSceneProtoInit with LayeredSceneLevelInit.
-}
initCompose : LayeredSceneProtoInit cdata userdata tar msg scenemsg idata -> LayeredSceneLevelInit userdata scenemsg idata -> LayeredSceneInit cdata userdata tar msg scenemsg
initCompose pinit linit env msg =
    pinit env <| linit env msg
