module Messenger.Scene.RawScene exposing (genRawScene)

{-|


# RawScene

Raw Scene is a scene without anything. Users can add whatever they like in the raw scene!

@docs genRawScene

-}

import Messenger.Scene.Loader exposing (SceneStorage)
import Messenger.Scene.Scene exposing (MConcreteScene, abstract)


{-| generate a raw scene from a concrete scene
-}
genRawScene : MConcreteScene data userdata scenemsg -> SceneStorage userdata scenemsg
genRawScene =
    abstract
