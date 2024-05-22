module Messenger.Base exposing
    ( WorldEvent(..)
    , UserEvent(..)
    , GlobalData, InternalData
    , Env
    , Flags
    , removeCommonData, addCommonData
    , UserViewGlobalData
    , emptyInternalData, userGlobalDataToGlobalData, globalDataToUserGlobalData
    , loadedResourceNum
    )

{-|


# Base Module

Some Basic Data Types for the game

@docs WorldEvent
@docs UserEvent
@docs GlobalData, InternalData, loadedSpriteNum
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
import Messenger.Audio.Internal exposing (AudioRepo, emptyRepo)
import Set exposing (Set)
import Time exposing (millisToPosix)


{-| World Event

This is the World Event for the game.

The events that messenger will receive from outside

Basically users don't need to deal with the world events, they work with user events instead.

-}
type WorldEvent
    = WTick Time.Posix
    | WKeyDown Int
    | WKeyUp Int
    | NewWindowSize ( Float, Float )
    | WindowVisibility Visibility
    | SoundLoaded String (Result Audio.LoadError Audio.Source)
    | TextureLoaded String (Maybe Texture)
    | WMouseDown Int ( Float, Float )
    | WMouseUp Int ( Float, Float )
    | MouseMove ( Float, Float )
    | WMouseWheel Int
    | WPrompt String String
    | NullEvent


{-| User Event

This is the User Event for the game.

Users can get outside information through these events.

`Tick` is triggered every timeInterval.

`KeyDown`, `KeyUp` records the keyboard events.
check all the keycodes [here](https://www.toptal.com/developers/keycode).

`MouseDown`, `MouseUp` records the button code and position when mouse up and down.
Mouse code 0 represents the left mouse button, 1 represents middle mouse button and 2 represents
right mouse button.

We have provide some key and mouse codes in messenger-extra.

`MouseWheel` records the wheel event for mouse, positive value means sroll down while
negative value means scroll up. It can be also used for touchpad.

-}
type UserEvent
    = Tick Int
    | KeyDown Int
    | KeyUp Int
    | MouseDown Int ( Float, Float )
    | MouseUp Int ( Float, Float )
    | MouseWheel Int
    | Prompt String String


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
    , globalStartTime : Int
    , sceneStartFrame : Int
    , globalStartFrame : Int
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


{-| This type is for user to use when initializing the messenger.
-}
type alias UserViewGlobalData userdata =
    { sceneStartTime : Int
    , globalStartTime : Int
    , sceneStartFrame : Int
    , globalStartFrame : Int
    , volume : Float
    , extraHTML : Maybe (Html WorldEvent)
    , canvasAttributes : List (Html.Attribute WorldEvent)
    , userData : userdata
    }


{-| Get the number of loaded sprites.
-}
loadedResourceNum : GlobalData userdata -> Int
loadedResourceNum globalData =
    Dict.size globalData.internalData.sprites + Dict.size globalData.internalData.audioRepo.audio


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
    , audioRepo = emptyRepo
    }


{-| Translate UserViewGlobalData to GlobalData
-}
userGlobalDataToGlobalData : UserViewGlobalData userdata -> GlobalData userdata
userGlobalDataToGlobalData user =
    { internalData = emptyInternalData
    , currentTimeStamp = millisToPosix 0
    , sceneStartTime = user.sceneStartTime
    , globalStartTime = user.globalStartTime
    , sceneStartFrame = user.sceneStartFrame
    , globalStartFrame = user.globalStartFrame
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
    , globalStartTime = globalData.globalStartTime
    , sceneStartFrame = globalData.sceneStartFrame
    , globalStartFrame = globalData.globalStartFrame
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
    , audioRepo : AudioRepo
    }


{-| The main flags.

Get info from js script.

**Learn more about flags [here](https://guide.elm-lang.org/interop/flags)**

-}
type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    , timeStamp : Int
    , info : String
    }
