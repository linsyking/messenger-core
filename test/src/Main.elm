module Main exposing (main)

{-|


# Main

Main module for the whole game.

@docs main

-}

import GlobalComponents exposing (allGlobalComponents)
import Lib.Base exposing (SceneMsg)
import Lib.Ports exposing (alert, audioPortFromJS, audioPortToJS, execREGLCmd, prompt, promptReceiver, recvREGLCmd, reglupdate, sendInfo, setView)
import Lib.Resources exposing (resources)
import Lib.UserData exposing (UserData)
import MainConfig exposing (debug, enabledBuiltinPrograms, fboNum, initGlobalData, initScene, initSceneMsg, saveGlobalData, timeInterval, virtualSize)
import Messenger.UI exposing (Output, genMain)
import Messenger.UserConfig exposing (EnabledBuiltinProgram(..), UserConfig)
import Scenes.AllScenes exposing (allScenes)


{-| User Configuration
-}
userConfig : UserConfig UserData SceneMsg
userConfig =
    { initScene = initScene
    , initSceneMsg = initSceneMsg
    , virtualSize = virtualSize
    , debug = debug
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
        , reglupdate = reglupdate
        , setView = setView
        , execREGLCmd = execREGLCmd
        , recvREGLCmd = recvREGLCmd
        }
    , enabledProgram = enabledBuiltinPrograms
    , fboNum = fboNum
    }


{-| Main
-}
main : Output UserData SceneMsg
main =
    genMain { config = userConfig, scenes = allScenes, resources = resources, globalComponents = allGlobalComponents }
