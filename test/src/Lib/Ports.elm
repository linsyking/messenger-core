port module Lib.Ports exposing
    ( audioPortFromJS, audioPortToJS
    , alert, prompt, promptReceiver, sendInfo
    , setView, execREGLCmd, recvREGLCmd, reglupdate
    )

{-|


# Ports

The ports that will be used in the game.

@docs audioPortFromJS, audioPortToJS
@docs alert, prompt, promptReceiver, sendInfo
@docs setView, execREGLCmd, recvREGLCmd, reglupdate

-}

import Json.Decode as Decode
import Json.Encode as Encode


{-| Port to send user data
-}
port sendInfo : String -> Cmd msg


{-| Port used by audio system
-}
port audioPortToJS : Encode.Value -> Cmd msg


{-| Port used by audio system
-}
port audioPortFromJS : (Decode.Value -> msg) -> Sub msg


{-| Port to alert
-}
port alert : String -> Cmd msg


{-| Port to prompt
-}
port prompt : { name : String, title : String } -> Cmd msg


{-| Port to receive prompt
-}
port promptReceiver : ({ name : String, result : String } -> msg) -> Sub msg


{-| Port to set view
-}
port setView : Encode.Value -> Cmd msg


{-| Port to execute REGL command
-}
port execREGLCmd : Encode.Value -> Cmd msg


{-| Port to receive REGL command result
-}
port recvREGLCmd : (Encode.Value -> msg) -> Sub msg


{-| Port to update REGL
-}
port reglupdate : (Float -> msg) -> Sub msg
