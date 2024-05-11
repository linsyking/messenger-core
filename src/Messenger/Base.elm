module Messenger.Base exposing (..)

import Audio
import Browser.Events exposing (Visibility)
import Canvas.Texture exposing (Texture)
import Dict exposing (Dict)
import Html exposing (Html)
import Messenger.Audio.Base exposing (AudioOption)
import Set exposing (Set)
import Time


{-| Msg

This is the msg data for main.

`Tick` records the time.

`KeyDown`, `KeyUp` records the keyboard events

`MouseWheel` records the wheel event for mouse, it can be also used for touchpad

-}
type WorldEvent
    = Tick Time.Posix
    | KeyDown Int
    | KeyUp Int
    | NewWindowSize ( Float, Float )
    | WindowVisibility Visibility
    | SoundLoaded String AudioOption (Result Audio.LoadError Audio.Source)
    | PlaySoundGotTime String AudioOption Audio.Source Time.Posix
    | TextureLoaded String (Maybe Texture)
    | MouseDown Int ( Float, Float )
    | MouseUp Int ( Float, Float )
    | MouseMove ( Float, Float )
    | MouseWheel Int
    | Prompt String String
    | NullEvent


{-| GlobalData

GD is the data that doesn't change during the game.

It won't be reset if you change the scene.

It is mainly used for display and reading/writing some localstorage data.

`browserViewPort` records the browser size.

`sprites` records all the sprites(images).

`localstorage` records the data that we save in localstorage.

`extraHTML` is used to render extra HTML tags. Be careful to use this.

`windowVisibility` records whether users stay in this tab/window

-}
type alias GlobalData userdata =
    { internalData : InternalData
    , sceneStartTime : Int
    , globalTime : Int
    , currentTimeStamp : Time.Posix
    , windowVisibility : Visibility
    , mousePos : ( Float, Float )
    , pressedKeys : Set Int
    , extraHTML : Maybe (Html WorldEvent)
    , volume : Float
    , userData : userdata
    , currentScene : String
    }


type alias Env common userdata =
    { globalData : GlobalData userdata
    , commonData : common
    }


type alias InternalData =
    { browserViewPort : ( Float, Float )
    , realWidth : Float
    , realHeight : Float
    , startLeft : Float
    , startTop : Float
    , sprites : Dict String Texture
    , virtualWidth : Float
    , virtualHeight : Float
    }


{-| Flags

The main flags.

Get info from js script

-}
type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    , timeStamp : Int
    , info : String
    }
