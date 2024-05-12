module Messenger.Base exposing
    ( WorldEvent(..)
    , GlobalData, InternalData
    , Env
    , Flags
    , removeCommonData, addCommonData
    , UserViewGlobalData
    , emptyInternalData, userGlobalDataToGlobalData, globalDataToUserGlobalData
    )

{-|


# Base Module

Some Basic Data Types for the game

@docs WorldEvent
@docs GlobalData, InternalData
@docs Env
@docs Flags
@docs removeCommonData, addCommonData
@docs UserViewGlobalData
@docs emptyInternalData, userGlobalDataToGlobalData, globalDataToUserGlobalData

-}

import Audio
import Browser.Events exposing (Visibility(..))
import Canvas.Texture exposing (Texture)
import Dict exposing (Dict)
import Html exposing (Html)
import Messenger.Audio.Base exposing (AudioOption)
import Set exposing (Set)
import Time exposing (millisToPosix)


{-| World Event

This is the World Event for the game.

Users can get outside information through the events.

`Tick` records the time.

`KeyDown`, `KeyUp` records the keyboard events

`MouseDown`, `MouseUp` records the button code and position when mouse up and down

`MouseWheel` records the wheel event for mouse, it can be also used for touchpad

**Note: Do Not use MouseMove Event to get mouse position. Use Globaldata instead.**

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

GlobalData is the data that doesn't change during the game.

It won't be reset if you change the scene.

It is mainly used for display and reading/writing some localstorage data.

  - `globalTime` records the past frames number since the game started
  - `sceneStartTime` records the past frames number since this scene started
  - `userdata` records the data that users set to save
  - `extraHTML` is used to render extra HTML tags. Be careful to use this
  - `windowVisibility` records whether users stay in this tab/window
  - `pressedKeys` records the keycodes that are be pressed now
  - `pressedMouseButtons` records the mouse buttons that are pressed now

-}
type alias GlobalData userdata =
    { internalData : InternalData
    , sceneStartTime : Int
    , globalTime : Int
    , currentTimeStamp : Time.Posix
    , windowVisibility : Visibility
    , mousePos : ( Float, Float )
    , pressedMouseButtons : Set Int
    , pressedKeys : Set Int
    , extraHTML : Maybe (Html WorldEvent)
    , canvasAttributes : List (Html.Attribute WorldEvent)
    , volume : Float
    , userData : userdata
    , currentScene : String
    }


{-| UserViewGlobalData

This type is for user to use when initializing the messenger.

-}
type alias UserViewGlobalData userdata =
    { sceneStartTime : Int
    , globalTime : Int
    , volume : Float
    , extraHTML : Maybe (Html WorldEvent)
    , canvasAttributes : List (Html.Attribute WorldEvent)
    , userData : userdata
    }


{-| Empty InternalData
-}
emptyInternalData : InternalData
emptyInternalData =
    { browserViewPort = ( 0, 0 )
    , realHeight = 0
    , realWidth = 0
    , startLeft = 0
    , startTop = 0
    , sprites = Dict.empty
    , virtualWidth = 0
    , virtualHeight = 0
    }


{-| Translate UserViewGlobalData to GlobalData
-}
userGlobalDataToGlobalData : UserViewGlobalData userdata -> GlobalData userdata
userGlobalDataToGlobalData user =
    { internalData = emptyInternalData
    , currentTimeStamp = millisToPosix 0
    , sceneStartTime = user.sceneStartTime
    , globalTime = user.globalTime
    , volume = user.volume
    , windowVisibility = Visible
    , pressedKeys = Set.empty
    , pressedMouseButtons = Set.empty
    , canvasAttributes = user.canvasAttributes
    , mousePos = ( 0, 0 )
    , extraHTML = user.extraHTML
    , userData = user.userData
    , currentScene = ""
    }


{-| Translate GlobalData to UserViewGlobalData
-}
globalDataToUserGlobalData : GlobalData userdata -> UserViewGlobalData userdata
globalDataToUserGlobalData globalData =
    { sceneStartTime = globalData.sceneStartTime
    , globalTime = globalData.globalTime
    , volume = globalData.volume
    , extraHTML = globalData.extraHTML
    , canvasAttributes = globalData.canvasAttributes
    , userData = globalData.userData
    }


{-| Environment

Environment is provided to users almost all the time.

It stores GlobalData and CommonData (Similar to GlobalData but just for one scene),
so you can get and modify them through the Env.

-}
type alias Env common userdata =
    { globalData : GlobalData userdata
    , commonData : common
    }


{-| Remove common data from environment.

Useful when dealing with portable components by yourself.

Most of the time it will not be used since it has been built into prepared update functions.

-}
removeCommonData : Env cdata userdata -> Env () userdata
removeCommonData env =
    { globalData = env.globalData
    , commonData = ()
    }


{-| Add the common data to a Environment without common data.
-}
addCommonData : cdata -> Env () userdata -> Env cdata userdata
addCommonData commonData env =
    { globalData = env.globalData
    , commonData = commonData
    }


{-| Internal GlobalData

Basically users do not need to get or modify them.

  - `browserViewPort` records the browser size.
  - `sprites` records all the sprites(images).

-}
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
Get info from js script.

**Learn more about flags [here](https://guide.elm-lang.org/interop/flags)**

-}
type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    , timeStamp : Int
    , info : String
    }
