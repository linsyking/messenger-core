module GlobalComponents exposing (allGlobalComopnents)

import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Scene.Scene exposing (GlobalComponentStorage)


{-| All global components to load at the beginning of the game.
-}
allGlobalComopnents : List (GlobalComponentStorage UserData SceneMsg)
allGlobalComopnents =
    []
