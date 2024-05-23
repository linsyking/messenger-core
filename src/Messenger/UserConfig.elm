module Messenger.UserConfig exposing
    ( TimeInterval(..)
    , UserConfig, PortDefs
    , coloredBackground, transparentBackground
    , Resources
    , resourceNum
    )

{-|


# User Configuration

@docs TimeInterval
@docs UserConfig, PortDefs
@docs coloredBackground, transparentBackground
@docs Resources
@docs resourceNum

-}

import Audio
import Browser.Events exposing (Visibility(..))
import Canvas exposing (Renderable)
import Canvas.Settings
import Color exposing (Color)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Messenger.Base exposing (GlobalData, UserViewGlobalData, WorldEvent)
import Messenger.Render.SpriteSheet exposing (SpriteSheet, spriteSheetSize)


{-| Time interval between every two frames.

  - `Fixed` represents the fixed time interval between every two frames.
    The value is the time interval in milliseconds.
  - `Animation` will use the browser's requestAnimationFrame to update the game.
    This will make the animation looks smoother.

-}
type TimeInterval
    = Fixed Float
    | Animation


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
    , background : GlobalData userdata -> Renderable
    , timeInterval : TimeInterval
    , ports : PortDefs
    }


{-| Resources

  - `allTexture` stores all the texture assets users will use in the game. The path is based on the project folder.
    **format: Dict name path**
  - `allSpriteSheets` stores all the sprite sheets users set for this game. users should both
    name the sprite sheets and every single sprite. Using it by **format: "sheet\_name.sprite\_name"**
    Sprite sheets are useful when managing the art recourses or making frame-by-frame animations
  - `allAudio` stores all the audio assets users will use in the game. **format: Dict name path**

-}
type alias Resources =
    { allTexture : Dict String String
    , allAudio : Dict String String
    , allSpriteSheets : SpriteSheet
    }


{-| The number of sprites in the game.
-}
resourceNum : Resources -> Int
resourceNum resources =
    Dict.size resources.allTexture + spriteSheetSize resources.allSpriteSheets + Dict.size resources.allAudio


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
