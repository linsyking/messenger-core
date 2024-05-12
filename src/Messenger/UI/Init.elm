module Messenger.UI.Init exposing (init, emptyInternalData)

{-|


# Game Init

Initialize the game

@docs init, emptyInternalData

-}

import Audio exposing (AudioCmd)
import Browser.Events exposing (Visibility(..))
import Canvas
import Dict
import Messenger.Base exposing (Env, Flags, GlobalData, InternalData, WorldEvent(..))
import Messenger.Coordinate.Coordinates exposing (getStartPoint, maxHandW)
import Messenger.Model exposing (Model)
import Messenger.Scene.Loader exposing (loadSceneByName)
import Messenger.Scene.Scene exposing (AbstractScene(..), AllScenes, MAbstractScene, SceneOutputMsg)
import Messenger.UserConfig exposing (UserConfig)
import Time exposing (millisToPosix)


{-| Empty Scene
-}
emptyScene : MAbstractScene userdata scenemsg
emptyScene =
    let
        abstractRec _ =
            let
                updates : Env () userdata -> WorldEvent -> ( MAbstractScene userdata scenemsg, List (SceneOutputMsg scenemsg userdata), Env () userdata )
                updates env _ =
                    ( abstractRec (), [], env )
            in
            Roll
                { update = updates
                , view = \_ -> Canvas.empty
                }
    in
    abstractRec ()


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


{-| Empty GlobalData
-}
emptyGlobalData : UserConfig userdata scenemsg -> GlobalData userdata
emptyGlobalData config =
    config.globalDataCodec.decode ""


{-| Initial model
-}
initModel : UserConfig userdata scenemsg -> Model userdata scenemsg
initModel config =
    { currentScene = emptyScene
    , currentGlobalData = emptyGlobalData config
    , audiorepo = []
    , transition = Nothing
    }


{-| Init

The Init function for the game

-}
init : UserConfig userdata scenemsg -> AllScenes userdata scenemsg -> Flags -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
init config scenes flags =
    let
        im =
            initModel config

        ms =
            loadSceneByName config.initScene scenes config.initSceneMsg { im | currentGlobalData = newgd }

        ( gw, gh ) =
            maxHandW oldgd ( flags.windowWidth, flags.windowHeight )

        ( fl, ft ) =
            getStartPoint oldgd ( flags.windowWidth, flags.windowHeight )

        oldIT =
            { emptyInternalData
                | virtualWidth = config.virtualSize.width
                , virtualHeight = config.virtualSize.height
            }

        oldgd =
            { initGlobalData | internalData = oldIT }

        newIT =
            { oldIT
                | browserViewPort = ( flags.windowWidth, flags.windowHeight )
                , realWidth = gw
                , realHeight = gh
                , startLeft = fl
                , startTop = ft
            }

        initGlobalData =
            config.globalDataCodec.decode flags.info

        newgd =
            { initGlobalData | currentTimeStamp = millisToPosix flags.timeStamp, internalData = newIT, currentScene = config.initScene }
    in
    ( { ms | currentGlobalData = newgd }, Cmd.none, Audio.cmdNone )
