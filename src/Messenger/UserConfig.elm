module Messenger.UserConfig exposing (UserConfig, PortDefs, coloredBackground, transparentBackground)

{-|


# User Configuration

@docs UserConfig, PortDefs, coloredBackground, transparentBackground

-}

import Audio
import Browser.Events exposing (Visibility(..))
import Canvas exposing (Renderable)
import Canvas.Settings
import Color exposing (Color)
import Json.Decode as Decode
import Json.Encode as Encode
import Messenger.Base exposing (GlobalData, WorldEvent)
import Messenger.Render.SpriteSheet exposing (SpriteSheet)


{-| User Configuration for the messenger.
-}
type alias UserConfig userdata scenemsg =
    { initScene : String
    , initSceneMsg : Maybe scenemsg
    , globalDataCodec :
        { encode : GlobalData userdata -> String
        , decode : String -> GlobalData userdata
        }
    , virtualSize :
        { width : Float
        , height : Float
        }
    , debug : Bool
    , background : GlobalData userdata -> Renderable
    , allTexture : List ( String, String )
    , allSpriteSheets : SpriteSheet
    , timeInterval : Float
    , ports : PortDefs
    }


{-| The ports that the user must provide to the messenger.
-}
type alias PortDefs =
    { sendInfo : String -> Cmd WorldEvent
    , audioPortToJS : Encode.Value -> Cmd (Audio.Msg WorldEvent)
    , audioPortFromJS : (Decode.Value -> Audio.Msg WorldEvent) -> Sub (Audio.Msg WorldEvent)
    , alert : String -> Cmd WorldEvent
    , prompt : { name : String, title : String } -> Cmd WorldEvent
    , promptReceiver : ({ name : String, result : String } -> WorldEvent) -> Sub WorldEvent
    }


{-| A transparent background. You should use this for release.
-}
transparentBackground : GlobalData userdata -> Renderable
transparentBackground gd =
    Canvas.clear ( 0, 0 ) gd.internalData.realWidth gd.internalData.realHeight


{-| A colored background.
-}
coloredBackground : Color -> GlobalData userdata -> Renderable
coloredBackground color gd =
    Canvas.shapes
        [ Canvas.Settings.fill color
        ]
        [ Canvas.rect ( 0, 0 ) gd.internalData.realWidth gd.internalData.realHeight
        ]
