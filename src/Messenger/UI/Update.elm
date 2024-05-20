module Messenger.UI.Update exposing (update)

{-|


# Game Update

Update the game

@docs update

-}

import Audio exposing (AudioCmd, AudioData)
import Canvas.Texture
import Dict
import Messenger.Audio.Audio exposing (loadAudio)
import Messenger.Base exposing (Env, UserEvent(..), WorldEvent(..), eventFilter, loadedSpriteNum)
import Messenger.Coordinate.Coordinates exposing (fromMouseToVirtual, getStartPoint, maxHandW)
import Messenger.Model exposing (Model, resetSceneStartTime, updateSceneTime)
import Messenger.Resources.Base exposing (saveSprite)
import Messenger.Scene.Loader exposing (existScene, loadSceneByName)
import Messenger.Scene.Scene exposing (AllScenes, unroll)
import Messenger.UI.SOMHandler exposing (handleSOM)
import Messenger.UserConfig exposing (UserConfig, spriteNum)
import Set
import Task
import Time


{-| Main logic for updating the game.
-}
gameUpdate : UserConfig userdata scenemsg -> AllScenes userdata scenemsg -> UserEvent -> Model userdata scenemsg -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
gameUpdate config scenes evnt model =
    if loadedSpriteNum model.currentGlobalData < spriteNum config.allTexture config.allSpriteSheets then
        -- Still loading assets
        ( model, Cmd.none, Audio.cmdNone )

    else
        let
            somHandler =
                handleSOM config scenes

            ( sdt, som, newenv ) =
                (unroll model.currentScene).update (Env model.currentGlobalData ()) evnt

            updatedModel1 =
                { model | currentGlobalData = newenv.globalData, currentScene = sdt }

            timeUpdatedModel =
                case evnt of
                    Tick ->
                        -- Tick event needs to update time
                        updateSceneTime updatedModel1

                    _ ->
                        updatedModel1

            ( updatedModel2, cmds, audiocmds ) =
                List.foldl
                    (\singleSOM ( lastModel, lastCmds, lastAudioCmds ) ->
                        let
                            ( newModel, newCmds, newAudioCmds ) =
                                somHandler singleSOM lastModel
                        in
                        ( newModel, newCmds ++ lastCmds, newAudioCmds ++ lastAudioCmds )
                    )
                    ( timeUpdatedModel, [], [] )
                    som

            updatedModel3 =
                case updatedModel2.transition of
                    Just ( trans, ( name, tm ) ) ->
                        if trans.currentTransition == trans.outT then
                            loadSceneByName name scenes tm updatedModel2
                                |> resetSceneStartTime

                        else
                            updatedModel2

                    Nothing ->
                        updatedModel2
        in
        ( updatedModel3
        , Cmd.batch cmds
        , Audio.cmdBatch audiocmds
        )


{-| Update

update function for the game

-}
update : UserConfig userdata scenemsg -> AllScenes userdata scenemsg -> AudioData -> WorldEvent -> Model userdata scenemsg -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
update config scenes _ msg model =
    let
        gd =
            model.currentGlobalData
    in
    case msg of
        TextureLoaded name Nothing ->
            ( model, config.ports.alert ("Failed to load sprite " ++ name), Audio.cmdNone )

        TextureLoaded name (Just t) ->
            let
                newgd =
                    case Dict.get name config.allSpriteSheets of
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
                                oldIT =
                                    gd.internalData

                                newIT =
                                    { oldIT | sprites = saveSprite oldIT.sprites name t }
                            in
                            { gd | internalData = newIT }
            in
            ( { model | currentGlobalData = newgd }, Cmd.none, Audio.cmdNone )

        SoundLoaded name opt result ->
            case result of
                Ok sound ->
                    ( model
                    , Task.perform (PlaySoundGotTime name opt sound) Time.now
                    , Audio.cmdNone
                    )

                Err _ ->
                    ( model
                    , config.ports.alert ("Failed to load audio " ++ name)
                    , Audio.cmdNone
                    )

        PlaySoundGotTime name opt sound t ->
            ( { model | audiorepo = loadAudio model.audiorepo name sound opt t }, Cmd.none, Audio.cmdNone )

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
                    fromMouseToVirtual gd ( px, py )
            in
            ( { model | currentGlobalData = { gd | mousePos = mp } }, Cmd.none, Audio.cmdNone )

        WMouseDown e pos ->
            let
                newPressedMouseButtons =
                    Set.insert e gd.pressedMouseButtons

                newModel =
                    { model | currentGlobalData = { gd | pressedMouseButtons = newPressedMouseButtons } }
            in
            gameUpdate config scenes (MouseDown e <| fromMouseToVirtual newModel.currentGlobalData pos) newModel

        WMouseUp e pos ->
            let
                newPressedMouseButtons =
                    Set.remove e gd.pressedMouseButtons

                newModel =
                    { model | currentGlobalData = { gd | pressedMouseButtons = newPressedMouseButtons } }
            in
            gameUpdate config scenes (MouseUp e <| fromMouseToVirtual newModel.currentGlobalData pos) newModel

        WKeyDown 112 ->
            if config.debug then
                -- F1
                ( model, config.ports.prompt { name = "load", title = "Enter the scene you want to load" }, Audio.cmdNone )

            else
                gameUpdate config scenes (KeyDown 112) model

        WKeyDown 113 ->
            if config.debug then
                -- F2
                ( model, config.ports.prompt { name = "setVolume", title = "Set volume (0-1)" }, Audio.cmdNone )

            else
                gameUpdate config scenes (KeyDown 113) model

        WKeyUp key ->
            let
                newPressedKeys =
                    Set.remove key gd.pressedKeys
            in
            gameUpdate config scenes (KeyUp key) { model | currentGlobalData = { gd | pressedKeys = newPressedKeys } }

        WKeyDown key ->
            let
                newPressedKeys =
                    Set.insert key gd.pressedKeys
            in
            gameUpdate config scenes (KeyDown key) { model | currentGlobalData = { gd | pressedKeys = newPressedKeys } }

        Prompt "load" result ->
            if existScene result scenes then
                ( loadSceneByName result scenes Nothing model
                    |> resetSceneStartTime
                , Cmd.none
                , Audio.cmdNone
                )

            else
                ( model, config.ports.alert "Scene not found!", Audio.cmdNone )

        Prompt "setVolume" result ->
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

        WTick x ->
            let
                newGD =
                    { gd | currentTimeStamp = x, globalTime = gd.globalTime + 1 }

                trans =
                    model.transition

                newTrans =
                    case trans of
                        Just ( data, sd ) ->
                            if data.currentTransition >= data.inT + data.outT then
                                Nothing

                            else
                                Just ( { data | currentTransition = data.currentTransition + 1 }, sd )

                        Nothing ->
                            trans
            in
            gameUpdate config scenes Tick { model | currentGlobalData = newGD, transition = newTrans }

        NullEvent ->
            ( model, Cmd.none, Audio.cmdNone )

        _ ->
            case eventFilter msg of
                Just umsg ->
                    gameUpdate config scenes umsg model

                Nothing ->
                    ( model, Cmd.none, Audio.cmdNone )
