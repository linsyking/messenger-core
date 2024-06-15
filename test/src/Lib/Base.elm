module Lib.Base exposing (SceneMsg(..))

{-|


# Base

Base module for the game. Set the UserData and SceneMsg here.

@docs SceneMsg

-}


{-| SceneMsg

`SceneMsg` represents the message type users wants
to send to a scene when switching scenes.

-}
type SceneMsg
    = NullSceneMsg
