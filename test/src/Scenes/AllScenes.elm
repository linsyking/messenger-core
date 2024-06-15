module Scenes.AllScenes exposing (allScenes)

{-|


# AllScenes

Record all the scenes here

@docs allScenes

-}

import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Scene.Scene exposing (AllScenes)
import Scenes.Home.Model as Home


{-| All Scenes

Store all the scenes with their name here.

-}
allScenes : AllScenes UserData SceneMsg
allScenes =
    [ ( "Home", Home.scene )
    ]
