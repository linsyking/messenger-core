module Messenger.Scene.RawScene exposing (..)

import Messenger.Scene.Loader exposing (SceneStorage)
import Messenger.Scene.Scene exposing (MConcreteScene, abstract)


genRawScene : MConcreteScene data userdata scenemsg -> SceneStorage userdata scenemsg
genRawScene =
    abstract
