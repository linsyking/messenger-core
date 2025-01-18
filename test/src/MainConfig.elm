module MainConfig exposing
    ( debug
    , initGlobalData
    , initScene
    , initSceneMsg
    , saveGlobalData
    , timeInterval
    , virtualSize
    , fboNum
    , enabledBuiltinPrograms
    )

{-|


# Main Config

@docs debug
@docs initGlobalData
@docs initScene
@docs initSceneMsg
@docs saveGlobalData
@docs timeInterval
@docs virtualSize
@docs fboNum
@docs enabledBuiltinPrograms

-}

import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData, decodeUserData, encodeUserData)
import Messenger.Base exposing (UserViewGlobalData)
import Messenger.UserConfig exposing (EnabledBuiltinProgram(..))
import REGL


{-| Initial scene
-}
initScene : String
initScene =
    "Home"


{-| Initial scene message
-}
initSceneMsg : Maybe SceneMsg
initSceneMsg =
    Nothing


{-| Virtual screen size
-}
virtualSize : { width : Float, height : Float }
virtualSize =
    { width = 1920, height = 1080 }


{-| Debug flag
-}
debug : Bool
debug =
    True


{-| Interval between two Tick messages
-}
timeInterval : REGL.TimeInterval
timeInterval =
    REGL.AnimationFrame


{-| Initialize the global data with the user data.

You may set the initial user data based on the user data.

-}
initGlobalData : String -> UserViewGlobalData UserData
initGlobalData data =
    let
        storage =
            decodeUserData data
    in
    { sceneStartTime = 0
    , sceneStartFrame = 0
    , globalStartTime = 0
    , globalStartFrame = 0
    , volume = 0.5
    , canvasAttributes = []
    , extraHTML = Nothing
    , userData = storage
    }


{-| Save Globaldata

Used when saving the user data to local storage.

-}
saveGlobalData : UserViewGlobalData UserData -> String
saveGlobalData globalData =
    encodeUserData globalData.userData


{-| The number of frame buffers used in the game.
-}
fboNum : Int
fboNum =
    5


{-| Builtin programs that are enabled.
-}
enabledBuiltinPrograms : EnabledBuiltinProgram
enabledBuiltinPrograms =
    AllBuiltinProgram
