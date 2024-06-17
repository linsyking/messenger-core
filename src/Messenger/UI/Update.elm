module Messenger.UI.Update exposing (update)

{-|


# Game Update

Update the game

@docs update

-}

import Audio exposing (AudioCmd, AudioData)
import Canvas.Texture
import Dict
import Messenger.Base exposing (Env, UserEvent(..), WorldEvent(..), loadedResourceNum, removeCommonData)
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
    if loadedResourceNum model.currentGlobalData < resourceNum input.resources then
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

            ( gc1, gcsompre, ( env1c, block ) ) =
                updateObjects (Env model.currentGlobalData model.currentScene) evnt model.globalComponents

            gcsom =
                filterSOM gcsompre

            env1 =
                removeCommonData env1c

            model1 : Model userdata scenemsg
            model1 =
                { currentScene = env1c.commonData, currentGlobalData = env1c.globalData, globalComponents = gc1 }

            ( scene1, scenesom, env2 ) =
                if block then
                    ( model1.currentScene, [], env1 )

                else
                    (unroll model1.currentScene).update env1 evnt

            model2 =
                { model1 | currentGlobalData = env2.globalData, currentScene = scene1 }

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
            model.currentGlobalData

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
            in
            ( { model | currentGlobalData = newgd }, Cmd.none, Audio.cmdNone )

        SoundLoaded name result ->
            case result of
                Ok sound ->
                    let
                        ar =
                            gdid.audioRepo

                        ard =
                            Dict.insert name ( sound, Audio.length audiodata sound ) ar.audio
                    in
                    ( { model | currentGlobalData = { gd | internalData = { gdid | audioRepo = { ar | audio = ard } } } }
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
            in
            ( { model | currentGlobalData = newgd }, Cmd.none, Audio.cmdNone )

        WindowVisibility v ->
            let
                newgd =
                    { gd | windowVisibility = v, pressedKeys = Set.empty, pressedMouseButtons = Set.empty }
            in
            ( { model | currentGlobalData = newgd }, Cmd.none, Audio.cmdNone )

        MouseMove ( px, py ) ->
            let
                mp =
                    fromMouseToVirtual gd.internalData ( px, py )
            in
            ( { model | currentGlobalData = { gd | mousePos = mp } }, Cmd.none, Audio.cmdNone )

        WMouseDown e pos ->
            let
                newPressedMouseButtons =
                    Set.insert e gd.pressedMouseButtons

                newModel =
                    { model | currentGlobalData = { gd | pressedMouseButtons = newPressedMouseButtons } }
            in
            gameUpdateInner (MouseDown e <| fromMouseToVirtual newModel.currentGlobalData.internalData pos) newModel

        WMouseUp e pos ->
            let
                newPressedMouseButtons =
                    Set.remove e gd.pressedMouseButtons

                newModel =
                    { model | currentGlobalData = { gd | pressedMouseButtons = newPressedMouseButtons } }
            in
            gameUpdateInner (MouseUp e <| fromMouseToVirtual newModel.currentGlobalData.internalData pos) newModel

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
            in
            gameUpdateInner (KeyUp key) { model | currentGlobalData = { gd | pressedKeys = newPressedKeys } }

        WKeyDown key ->
            let
                newPressedKeys =
                    Set.insert key gd.pressedKeys
            in
            gameUpdateInner (KeyDown key) { model | currentGlobalData = { gd | pressedKeys = newPressedKeys } }

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
                        newGd =
                            { gd | volume = v }
                    in
                    ( { model | currentGlobalData = newGd }, Cmd.none, Audio.cmdNone )

                Nothing ->
                    ( model, config.ports.alert "Not a number", Audio.cmdNone )

        WPrompt name result ->
            gameUpdateInner (Prompt name result) model

        WTick delta ->
            let
                timeInterval =
                    Time.posixToMillis delta - Time.posixToMillis gd.currentTimeStamp

                newGD =
                    { gd | currentTimeStamp = delta, globalStartFrame = gd.globalStartFrame + 1, globalStartTime = gd.globalStartTime + timeInterval }
            in
            gameUpdateInner (Tick timeInterval) { model | currentGlobalData = newGD }

        NullEvent ->
            ( model, Cmd.none, Audio.cmdNone )

        WMouseWheel x ->
            gameUpdateInner (MouseWheel x) model
