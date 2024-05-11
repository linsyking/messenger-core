module Messenger.Base exposing
    ( WorldEvent(..)
    , GlobalData, InternalData
    , Env
    , Flags
    )

{-|


# Base Module

Some Basic Data Types for the game

@docs WorldEvent
@docs GlobalData, InternalData
@docs Env
@docs Flags

-}

import Audio
import Browser.Events exposing (Visibility)
import Canvas.Texture exposing (Texture)
import Dict exposing (Dict)
import Html exposing (Html)
import Messenger.Audio.Base exposing (AudioOption)
import Set exposing (Set)
import Time


{-| World Event

This is the World Event for the game.
Users can get outside information throught the events.

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


{-| Environment

Environment is provided to users almost all the time.

It stores GlobalData and CommonData (Similar to GlobalData but just for one scene),
so you can get and modify them through the Env.

-}
type alias Env common userdata =
    { globalData : GlobalData userdata
    , commonData : common
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
