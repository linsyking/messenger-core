module Messenger.UI.Init exposing (init)

{-|


# Game Init

Initialize the game

@docs init

-}

import Audio exposing (AudioCmd)
import Browser.Events exposing (Visibility(..))
import Canvas
import Dict
import Messenger.Base exposing (Env, Flags, GlobalData, UserEvent, WorldEvent(..), emptyInternalData, userGlobalDataToGlobalData)
import Messenger.Coordinate.Coordinates exposing (getStartPoint, maxHandW)
import Messenger.Model exposing (Model)
import Messenger.Scene.Loader exposing (loadSceneByName)
import Messenger.Scene.Scene exposing (AbstractScene(..), MAbstractScene, SceneOutputMsg)
import Messenger.UI.Input exposing (Input)
import Messenger.UserConfig exposing (UserConfig)
import Time exposing (millisToPosix)


{-| Empty Scene
-}
emptyScene : MAbstractScene userdata scenemsg
emptyScene =
    let
        abstractRec _ =
            let
                updates : Env () userdata -> UserEvent -> ( MAbstractScene userdata scenemsg, List (SceneOutputMsg scenemsg userdata), Env () userdata )
                updates env _ =
                    ( abstractRec (), [], env )
            in
            Roll
                { update = updates
                , view = \_ -> Canvas.empty
                }
    in
    abstractRec ()


{-| Empty GlobalData
-}
emptyGlobalData : UserConfig userdata scenemsg -> GlobalData userdata
emptyGlobalData config =
    userGlobalDataToGlobalData (config.globalDataCodec.decode "")


{-| Initial model
-}
initModel : UserConfig userdata scenemsg -> Model userdata scenemsg
initModel config =
    { currentScene = emptyScene
    , currentGlobalData = emptyGlobalData config
    , globalComponents = []
    }


{-| The Init function for the game.
-}
init : Input userdata scenemsg -> Flags -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
init input flags =
    let
        config =
            input.config

        scenes =
            input.scenes

        resources =
            input.resources

        im =
            initModel config

        ms =
            loadSceneByName config.initScene scenes config.initSceneMsg { im | currentGlobalData = newgd }

        ( gw, gh ) =
            maxHandW ( config.virtualSize.width, config.virtualSize.height ) ( flags.windowWidth, flags.windowHeight )

        ( fl, ft ) =
            getStartPoint ( config.virtualSize.width, config.virtualSize.height ) ( flags.windowWidth, flags.windowHeight )

        newIT =
            { emptyInternalData
                | browserViewPort = ( flags.windowWidth, flags.windowHeight )
                , realWidth = gw
                , realHeight = gh
                , startLeft = fl
                , startTop = ft
                , virtualWidth = config.virtualSize.width
                , virtualHeight = config.virtualSize.height
            }

        initGlobalData =
            userGlobalDataToGlobalData (config.globalDataCodec.decode flags.info)

        newgd =
            { initGlobalData | currentTimeStamp = millisToPosix flags.timeStamp, internalData = newIT, currentScene = config.initScene }

        audioLoad =
            List.map
                (\( name, url ) ->
                    Audio.loadAudio (SoundLoaded name) url
                )
                (Dict.toList resources.allAudio)

        gcs =
            List.map (\gc -> gc (Env newgd ms.currentScene)) input.globalComponents
    in
    ( { ms | currentGlobalData = newgd, globalComponents = gcs }, Cmd.none, Audio.cmdBatch audioLoad )
