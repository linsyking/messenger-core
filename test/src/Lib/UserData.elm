module Lib.UserData exposing (UserData, decodeUserData, encodeUserData)

{-|


# User data

@docs UserData, decodeUserData, encodeUserData

-}

import Json.Decode as Decode exposing (at, decodeString)
import Json.Encode as Encode


{-| User defined data
-}
type alias UserData =
    {}


{-| Encoder for the UserData.
-}
encodeUserData : UserData -> String
encodeUserData storage =
    Encode.encode 0
        (Encode.object
            [--Aadd your data here
             -- Example:
             -- ( "volume", Encode.float storage.volume )
            ]
        )


{-| Decoder for the UserData.
-}
decodeUserData : String -> UserData
decodeUserData ls =
    -- Example:
    -- let
    --     vol =
    --         Result.withDefault 0.5 (decodeString (at [ "volume" ] Decode.float) ls)
    -- in
    UserData
