module Messenger.UserConfig exposing
    ( UserConfig, PortDefs
    , EnabledBuiltinProgram(..)
    )

{-|


# User Configuration

@docs UserConfig, PortDefs
@docs EnabledBuiltinProgram

-}

import Audio
import Json.Decode as Decode
import Json.Encode as Encode
import Messenger.Base exposing (UserViewGlobalData, WorldEvent)
import REGL


{-| Enabled Builtin Program

  - `NoBuiltinProgram` represents enabling no builtin program
  - `CustomBuiltinProgramList` represents enabling a list of custom builtin programs
  - `TextOnlyBuiltinProgram` represents enabling the builtin program for text only
  - `BasicShapesBuiltinProgram` represents enabling the builtin programs for basic shapes
  - `CommonBuiltinProgram` represents enabling the builtin programs for common shapes
  - `AllBuiltinProgram` represents enabling all builtin programs (Recommended)

-}
type EnabledBuiltinProgram
    = NoBuiltinProgram
    | CustomBuiltinProgramList (List String)
    | TextOnlyBuiltinProgram
    | BasicShapesBuiltinProgram
    | AllBuiltinProgram


{-| User Configuration for the messenger.

`userdata` is a custom type which can store any data in the game.
users can **save their own global data** and **implement local storage** here.

`scenemsg` is another custom type which represents the message type users wants
to send to a scene when switching scenes.

  - `initScene` represents the scene users get start
  - `initSceneMsg` represents the message to initialize the start scene
  - `globalDataCodec` is for local storage, users can encode the any data in global data
    and user data to storage them, and decode them when reopen the game.
    This globalData is a subset of the real global data, removing all internal data structures
  - `virtualSize` represents how users want their game be virtual sized. In other words,
    users make their game in the virtual size, and the game will be resized due to the browser window size
    but keeping the aspect ratio
  - `debug` option determines whether enable some simple debugging tools or not
    remember to disable it when releasing game
  - `background` determines the background of the game
    transparent background and colored background is already prepared
  - `timeInterval` See `TimeInterval`
  - `ports` stores the ports that users must provide.

-}
type alias UserConfig userdata scenemsg =
    { initScene : String
    , initSceneMsg : Maybe scenemsg
    , globalDataCodec :
        { encode : UserViewGlobalData userdata -> String
        , decode : String -> UserViewGlobalData userdata
        }
    , virtualSize :
        { width : Float
        , height : Float
        }
    , debug : Bool
    , timeInterval : REGL.TimeInterval
    , ports : PortDefs
    , enabledProgram : EnabledBuiltinProgram
    , fboNum : Int
    }


{-| The ports that the user must provide to the messenger.

**Learn more about ports [here](https://guide.elm-lang.org/interop/ports)**

-}
type alias PortDefs =
    { sendInfo : String -> Cmd WorldEvent
    , audioPortToJS : Encode.Value -> Cmd (Audio.Msg WorldEvent)
    , audioPortFromJS : (Decode.Value -> Audio.Msg WorldEvent) -> Sub (Audio.Msg WorldEvent)
    , alert : String -> Cmd WorldEvent
    , prompt : { name : String, title : String } -> Cmd WorldEvent
    , promptReceiver : ({ name : String, result : String } -> WorldEvent) -> Sub WorldEvent
    , reglupdate : (Float -> WorldEvent) -> Sub WorldEvent
    , setView : Encode.Value -> Cmd WorldEvent
    , execREGLCmd : Encode.Value -> Cmd WorldEvent
    , recvREGLCmd : (Encode.Value -> WorldEvent) -> Sub WorldEvent
    }
