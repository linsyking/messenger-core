module Messenger.UI.Update exposing (update)

{-|


# Game Update

Update the game

@docs update

-}

import Audio exposing (AudioCmd, AudioData)
import Canvas.Texture
import Dict
import Messenger.Base exposing (UserEvent(..), WorldEvent(..), addCommonData, loadedResourceNum, removeCommonData)
import Messenger.Component.GlobalComponent exposing (filterAliveGC)
import Messenger.Coordinate.Coordinates exposing (fromMouseToVirtual, getStartPoint, maxHandW)
import Messenger.GeneralModel exposing (filterSOM)
import Messenger.Model exposing (Model, resetSceneStartTime, updateSceneTime)
import Messenger.Recursion exposing (updateObjects)
import Messenger.Resources.Base exposing (saveSprite)
import Messenger.Scene.Loader exposing (existScene, loadSceneByName)
import Messenger.Scene.Scene exposing (unroll)
import Messenger.UI.Input exposing (Input)
import Messenger.UI.SOMHandler exposing (handleSOMs)
import Messenger.UserConfig exposing (resourceNum)
import Set
import Time


{-| Main logic for updating the game.
-}
gameUpdate : Input userdata scenemsg -> UserEvent -> Model userdata scenemsg -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
gameUpdate input evnt model =
    if loadedResourceNum model.env.globalData < resourceNum input.resources then
        -- Still loading assets
        ( model, Cmd.none, Audio.cmdNone )

    else
        let
            scenes =
                input.scenes

            config =
                input.config

            somHandler =
                handleSOMs config scenes

            gc1 =
                filterAliveGC model.globalComponents

            ( gc2, gcsompre, ( env2, block ) ) =
                updateObjects env1 evnt gc1

            gcsom =
                filterSOM gcsompre

            env1 =
                model.env

            model1 : Model userdata scenemsg
            model1 =
                { env = env2, globalComponents = gc2 }

            ( scenesom, model2 ) =
                if block then
                    ( [], model1 )

                else
                    let
                        ( scene, psom, env ) =
                            (unroll env2.commonData).update (removeCommonData env2) evnt

                        envc =
                            addCommonData scene env
                    in
                    ( psom, { model1 | env = envc } )

            -- GC SOM should be handled before Scene SOM
            som =
                gcsom ++ scenesom

            model3 =
                case evnt of
                    Tick delta ->
                        -- Tick event needs to update time
                        updateSceneTime model2 delta

                    _ ->
                        model2

            ( model4, cmds, audiocmds ) =
                somHandler som model3
        in
        ( model4
        , Cmd.batch cmds
        , Audio.cmdBatch audiocmds
        )


{-| Outer update function that handles world events
-}
update : Input userdata scenemsg -> AudioData -> WorldEvent -> Model userdata scenemsg -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
update input audiodata msg model =
    let
        gd =
            env.globalData

        env =
            model.env

        gdid =
            gd.internalData

        scenes =
            input.scenes

        config =
            input.config

        resources =
            input.resources

        gameUpdateInner =
            gameUpdate input
    in
    case msg of
        TextureLoaded name Nothing ->
            ( model, config.ports.alert ("Failed to load sprite " ++ name), Audio.cmdNone )

        TextureLoaded name (Just t) ->
            let
                newgd =
                    case Dict.get name resources.allSpriteSheets of
                        Just sprites ->
                            -- Save all sprites in the spritesheet
                            List.foldl
                                (\( n, s ) lastgd ->
                                    let
                                        ( x, y ) =
                                            s.realStartPoint

                                        ( w, h ) =
                                            s.realSize

                                        newTexture =
                                            Canvas.Texture.sprite { x = x, y = y, width = w, height = h } t

                                        oldIT =
                                            lastgd.internalData

                                        newIT =
                                            { oldIT | sprites = saveSprite oldIT.sprites (name ++ "." ++ n) newTexture }
                                    in
                                    { lastgd | internalData = newIT }
                                )
                                gd
                                sprites

                        Nothing ->
                            let
                                newIT =
                                    { gdid | sprites = saveSprite gdid.sprites name t }
                            in
                            { gd | internalData = newIT }

                newEnv =
                    { env | globalData = newgd }
            in
            ( { model | env = newEnv }, Cmd.none, Audio.cmdNone )

        SoundLoaded name result ->
            case result of
                Ok sound ->
                    let
                        ar =
                            gdid.audioRepo

                        ard =
                            Dict.insert name ( sound, Audio.length audiodata sound ) ar.audio

                        newEnv =
                            { env | globalData = { gd | internalData = { gdid | audioRepo = { ar | audio = ard } } } }
                    in
                    ( { model | env = newEnv }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                Err _ ->
                    ( model
                    , config.ports.alert ("Failed to load audio " ++ name)
                    , Audio.cmdNone
                    )

        NewWindowSize t ->
            let
                ( gw, gh ) =
                    maxHandW ( gd.internalData.virtualWidth, gd.internalData.virtualHeight ) t

                ( fl, ft ) =
                    getStartPoint ( gd.internalData.virtualWidth, gd.internalData.virtualHeight ) t

                oldIT =
                    gd.internalData

                newIT =
                    { oldIT | browserViewPort = t, realWidth = gw, realHeight = gh, startLeft = fl, startTop = ft }

                newgd =
                    { gd | internalData = newIT }

                newEnv =
                    { env | globalData = newgd }
            in
            ( { model | env = newEnv }, Cmd.none, Audio.cmdNone )

        WindowVisibility v ->
            let
                newgd =
                    { gd | windowVisibility = v, pressedKeys = Set.empty, pressedMouseButtons = Set.empty }

                newEnv =
                    { env | globalData = newgd }
            in
            ( { model | env = newEnv }, Cmd.none, Audio.cmdNone )

        MouseMove ( px, py ) ->
            let
                mp =
                    fromMouseToVirtual gd.internalData ( px, py )

                newEnv =
                    { env | globalData = { gd | mousePos = mp } }
            in
            ( { model | env = newEnv }, Cmd.none, Audio.cmdNone )

        WMouseDown e pos ->
            let
                newPressedMouseButtons =
                    Set.insert e gd.pressedMouseButtons

                newEnv =
                    { env | globalData = { gd | pressedMouseButtons = newPressedMouseButtons } }

                newModel =
                    { model | env = newEnv }
            in
            gameUpdateInner (MouseDown e <| fromMouseToVirtual newModel.env.globalData.internalData pos) newModel

        WMouseUp e pos ->
            let
                newPressedMouseButtons =
                    Set.remove e gd.pressedMouseButtons

                newEnv =
                    { env | globalData = { gd | pressedMouseButtons = newPressedMouseButtons } }

                newModel =
                    { model | env = newEnv }
            in
            gameUpdateInner (MouseUp e <| fromMouseToVirtual newModel.env.globalData.internalData pos) newModel

        WKeyDown 112 ->
            if config.debug then
                -- F1
                ( model, config.ports.prompt { name = "load", title = "Enter the scene you want to load" }, Audio.cmdNone )

            else
                gameUpdateInner (KeyDown 112) model

        WKeyDown 113 ->
            if config.debug then
                -- F2
                ( model, config.ports.prompt { name = "setVolume", title = "Set volume (0-1)" }, Audio.cmdNone )

            else
                gameUpdateInner (KeyDown 113) model

        WKeyUp key ->
            let
                newPressedKeys =
                    Set.remove key gd.pressedKeys

                newEnv =
                    { env | globalData = { gd | pressedKeys = newPressedKeys } }
            in
            gameUpdateInner (KeyUp key) { model | env = newEnv }

        WKeyDown key ->
            let
                newPressedKeys =
                    Set.insert key gd.pressedKeys

                newEnv =
                    { env | globalData = { gd | pressedKeys = newPressedKeys } }
            in
            gameUpdateInner (KeyDown key) { model | env = newEnv }

        WPrompt "load" result ->
            if existScene result scenes then
                ( loadSceneByName result scenes Nothing model
                    |> resetSceneStartTime
                , Cmd.none
                , Audio.cmdNone
                )

            else
                ( model, config.ports.alert "Scene not found!", Audio.cmdNone )

        WPrompt "setVolume" result ->
            let
                vol =
                    String.toFloat result
            in
            case vol of
                Just v ->
                    let
                        newgd =
                            { gd | volume = v }

                        newEnv =
                            { env | globalData = newgd }
                    in
                    ( { model | env = newEnv }, Cmd.none, Audio.cmdNone )

                Nothing ->
                    ( model, config.ports.alert "Not a number", Audio.cmdNone )

        WPrompt name result ->
            gameUpdateInner (Prompt name result) model

        WTick delta ->
            let
                timeInterval =
                    Time.posixToMillis delta - Time.posixToMillis gd.currentTimeStamp

                newgd =
                    { gd | currentTimeStamp = delta, globalStartFrame = gd.globalStartFrame + 1, globalStartTime = gd.globalStartTime + timeInterval }

                newEnv =
                    { env | globalData = newgd }
            in
            gameUpdateInner (Tick timeInterval) { model | env = newEnv }

        WMouseWheel x ->
            gameUpdateInner (MouseWheel x) model

        NullEvent ->
            ( model, Cmd.none, Audio.cmdNone )
