module Main exposing (main)

{-|


# Main

Main module for the whole game.

@docs main

-}

import Lib.Base exposing (SceneMsg)
import Lib.Ports exposing (alert, audioPortFromJS, audioPortToJS, prompt, promptReceiver, sendInfo)
import Lib.Resources exposing (resources)
import Lib.UserData exposing (UserData)
import MainConfig exposing (background, debug, initGlobalData, initScene, initSceneMsg, saveGlobalData, timeInterval, virtualSize)
import Messenger.UI exposing (Output, genMain)
import Messenger.UserConfig exposing (UserConfig)
import Scenes.AllScenes exposing (allScenes)


{-| User Configuration
-}
userConfig : UserConfig UserData SceneMsg
userConfig =
    { initScene = initScene
    , initSceneMsg = initSceneMsg
    , virtualSize = virtualSize
    , debug = debug
    , background = background
    , timeInterval = timeInterval
    , globalDataCodec =
        { encode = saveGlobalData
        , decode = initGlobalData
        }
    , ports =
        { sendInfo = sendInfo
        , audioPortToJS = audioPortToJS
        , audioPortFromJS = audioPortFromJS
        , alert = alert
        , prompt = prompt
        , promptReceiver = promptReceiver
        }
    }


{-| Main
-}
main : Output UserData SceneMsg
main =
    genMain { config = userConfig, scenes = allScenes, resources = resources }
