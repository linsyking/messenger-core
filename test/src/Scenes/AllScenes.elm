module Scenes.AllScenes exposing (allScenes)

{-|


# AllScenes

Record all the scenes here

@docs allScenes

-}

import Dict
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Scene.Scene exposing (AllScenes)
import Scenes.Audio.Model as Audio
import Scenes.Home.Model as Home
import Scenes.Stress.Model as Stress
import Scenes.Transition.Model as Transition


{-| All Scenes

Store all the scenes with their name here.

-}
allScenes : AllScenes UserData SceneMsg
allScenes =
    Dict.fromList
        [ ( "Home", Home.scene )
        , ( "Transition", Transition.scene )
        , ( "Stress", Stress.scene )
        , ( "Audio", Audio.scene )
        ]
