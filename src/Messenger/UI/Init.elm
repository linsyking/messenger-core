module Messenger.UI.Init exposing (init)

{-|


# Game Init

Initialize the game

@docs init

-}

import Audio exposing (AudioCmd)
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (Visibility(..))
import Dict
import Messenger.Base exposing (Env, Flags, GlobalData, UserEvent, WorldEvent(..), emptyInternalData, userGlobalDataToGlobalData)
import Messenger.Model exposing (Model)
import Messenger.Scene.Loader exposing (loadSceneByName)
import Messenger.Scene.Scene exposing (AbstractScene(..), MAbstractScene, SceneOutputMsg)
import Messenger.UI.Input exposing (Input)
import Messenger.UserConfig exposing (EnabledBuiltinProgram(..), UserConfig)
import REGL
import Task


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
                , view = \_ -> REGL.empty
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
    { env = Env (emptyGlobalData config) emptyScene
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

        env1 =
            im.env

        newEnv1 =
            { env1 | globalData = newgd }

        ms =
            loadSceneByName config.initScene scenes config.initSceneMsg { im | env = newEnv1 }

        newIT =
            { emptyInternalData
                | virtualWidth = config.virtualSize.width
                , virtualHeight = config.virtualSize.height
            }

        initGlobalData =
            userGlobalDataToGlobalData (config.globalDataCodec.decode flags.info)

        newgd =
            { initGlobalData | currentTimeStamp = flags.timeStamp, internalData = newIT, currentScene = config.initScene }

        audioLoad =
            List.map
                (\( name, url ) ->
                    Audio.loadAudio (SoundLoaded name) url
                )
                (Dict.toList resources.allAudio)

        gcs =
            List.map (\gc -> gc (Env newgd ms.env.commonData)) input.globalComponents

        env2 =
            ms.env

        newEnv2 =
            { env2 | globalData = newgd }

        loadtexturecmds =
            List.map
                (\key ->
                    let
                        ( url, opts ) =
                            Maybe.withDefault ( "", Nothing ) (Dict.get key resources.allTexture)
                    in
                    REGL.loadTexture key url opts
                )
                (Dict.keys resources.allTexture)

        loadfontcmds =
            List.map
                (\( key, url1, url2 ) ->
                    REGL.loadMSDFFont key url1 url2
                )
                resources.allFont

        loadprograms =
            List.map
                (\( key, program ) ->
                    REGL.createREGLProgram key program
                )
                resources.allProgram
    in
    ( { ms | env = newEnv2, globalComponents = gcs }
    , Cmd.batch <|
        Task.perform (\res -> NewWindowSize ( res.scene.width, res.scene.height )) getViewport
            :: (REGL.batchExec config.ports.execREGLCmd <|
                    REGL.startREGL (REGL.REGLStartConfig config.virtualSize.width config.virtualSize.height 5 (bulitinPrograms config.enabledProgram))
                        :: REGL.configREGL
                            (REGL.REGLConfig config.timeInterval)
                        :: (loadtexturecmds ++ loadfontcmds ++ loadprograms)
               )
    , Audio.cmdBatch audioLoad
    )


bulitinPrograms : EnabledBuiltinProgram -> Maybe (List String)
bulitinPrograms c =
    case c of
        CustomBuiltinProgramList xs ->
            Just xs

        NoBuiltinProgram ->
            Just []

        TextOnlyBuiltinProgram ->
            Just [ "textbox" ]

        BasicShapesBuiltinProgram ->
            Just [ "textbox", "triangle", "circle", "quad", "poly" ]

        AllBuiltinProgram ->
            Nothing
