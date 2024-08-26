module Messenger.Scene.LayeredScene exposing
    ( LayeredSceneData
    , genLayeredScene
    , LayeredSceneInit, LayeredSceneSettingsFunc
    , LayeredSceneProtoInit, LayeredSceneLevelInit, initCompose
    )

{-|


# Layered Scene

Layered scene is a pre-defined scene implementation provided by Messenger.
A layered scene can only handle a list of layers with fixed `cdata`, `userdata`, `tar`, `msg` and `scenemsg` types.

@docs LayeredSceneData
@docs genLayeredScene
@docs LayeredSceneInit, LayeredSceneSettingsFunc


## Scene Prototype

@docs LayeredSceneProtoInit, LayeredSceneLevelInit, initCompose

-}

import Canvas exposing (Renderable, group)
import Canvas.Settings exposing (Setting)
import Messenger.Base exposing (Env, UserEvent, addCommonData, removeCommonData)
import Messenger.GeneralModel exposing (MsgBase(..), filterSOM, viewModelList)
import Messenger.Layer.Layer exposing (AbstractLayer)
import Messenger.Recursion exposing (updateObjects)
import Messenger.Scene.Scene exposing (SceneOutputMsg, SceneStorage, abstract)


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
    { renderSettings : List Setting
    , commonData : cdata
    , layers : List (AbstractLayer cdata userdata tar msg scenemsg)
    }


updateLayeredScene : (Env () userdata -> UserEvent -> LayeredSceneData cdata userdata tar msg scenemsg -> List Setting) -> Env () userdata -> UserEvent -> LayeredSceneData cdata userdata tar msg scenemsg -> ( LayeredSceneData cdata userdata tar msg scenemsg, List (SceneOutputMsg scenemsg userdata), Env () userdata )
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


{-| init type sugar
-}
type alias LayeredSceneInit cdata userdata tar msg scenemsg =
    Env () userdata -> Maybe scenemsg -> LayeredSceneData cdata userdata tar msg scenemsg


{-| init type sugar for levels
-}
type alias LayeredSceneLevelInit userdata scenemsg idata =
    Env () userdata -> Maybe scenemsg -> Maybe idata


{-| init type sugar for scene prototypes
-}
type alias LayeredSceneProtoInit cdata userdata tar msg scenemsg idata =
    Env () userdata -> Maybe idata -> LayeredSceneData cdata userdata tar msg scenemsg


{-| settingsFunc type sugar
-}
type alias LayeredSceneSettingsFunc cdata userdata tar msg scenemsg =
    Env () userdata -> UserEvent -> LayeredSceneData cdata userdata tar msg scenemsg -> List Setting


{-| This creates a layered scene.

  - `init` creates the initial layered scene from env data and init msg.
  - `settingsFunc` is a user provided function to modify `renderSettings` each time the scene updates. If you don't need `settingsFunc`, simply provide a `func _ _ _ = settings`

-}
genLayeredScene : LayeredSceneInit cdata userdata tar msg scenemsg -> LayeredSceneSettingsFunc cdata userdata tar msg scenemsg -> SceneStorage userdata scenemsg
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
