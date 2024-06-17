module GlobalComponents exposing (allGlobalComopnents)

{-|


# Global Component Configuration

Record all the global components to load at the beginning here

@docs allGlobalComopnents

-}

import GlobalComponents.GC1.Model as FPS
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Scene.Scene exposing (GlobalComponentStorage)


{-| All global components to load at the beginning of the game.
-}
allGlobalComopnents : List (GlobalComponentStorage UserData SceneMsg)
allGlobalComopnents =
    [ FPS.genGC (Just <| FPS.encode (FPS.Msg 20)) Nothing
    ]
