module Messenger.UI.Update exposing (update)

{-|


# Game Update

Update the game

@docs update

-}

import Audio exposing (AudioCmd, AudioData)
import Canvas.Texture
import Dict
import Messenger.Audio.Audio exposing (loadAudio, stopAudio)
import Messenger.Base exposing (Env, WorldEvent(..))
import Messenger.Coordinate.Coordinates exposing (fromMouseToVirtual, getStartPoint, maxHandW)
import Messenger.Model exposing (Model, resetSceneStartTime, updateSceneTime)
import Messenger.Resources.Base exposing (saveSprite)
import Messenger.Scene.Loader exposing (SceneStorage, existScene, loadSceneByName)
import Messenger.Scene.Scene exposing (SceneOutputMsg(..), unroll)
import Messenger.UserConfig exposing (UserConfig)
import Set
import Task
import Time


{-| gameUpdate

main logic for updating the game

-}
gameUpdate : UserConfig userdata scenemsg -> List ( String, SceneStorage userdata scenemsg ) -> WorldEvent -> Model userdata scenemsg -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
gameUpdate config scenes evnt model =
    if List.length (Dict.keys model.currentGlobalData.internalData.sprites) < List.length config.allTexture then
        -- Still loading assets
        ( model, Cmd.none, Audio.cmdNone )

    else
        let
            ( sdt, som, newenv ) =
                (unroll model.currentScene).update (Env model.currentGlobalData ()) evnt

            updatedModel1 =
                { model | currentGlobalData = newenv.globalData, currentScene = sdt }

            timeUpdatedModel =
                case evnt of
                    Tick _ ->
                        -- Tick event needs to update time
                        updateSceneTime updatedModel1

                    _ ->
                        updatedModel1

            ( updatedModel2, cmds, audiocmds ) =
                List.foldl
                    (\singleSOM ( lastModel, lastCmds, lastAudioCmds ) ->
                        case singleSOM of
                            SOMChangeScene ( tm, name, Nothing ) ->
                                --- Load new scene
                                ( loadSceneByName name scenes tm lastModel
                                    |> resetSceneStartTime
                                , lastCmds
                                , lastAudioCmds
                                )

                            SOMChangeScene ( tm, s, Just trans ) ->
                                --- Delayed Loading
                                ( { lastModel | transition = Just ( trans, ( s, tm ) ) }
                                , lastCmds
                                , lastAudioCmds
                                )

                            SOMPlayAudio name path opt ->
                                ( lastModel, lastCmds, lastAudioCmds ++ [ Audio.loadAudio (SoundLoaded name opt) path ] )

                            SOMSetVolume s ->
                                let
                                    oldgd =
                                        lastModel.currentGlobalData

                                    newgd2 =
                                        { oldgd | volume = s }
                                in
                                ( { lastModel | currentGlobalData = newgd2 }, lastCmds, lastAudioCmds )

                            SOMStopAudio name ->
                                ( { lastModel | audiorepo = stopAudio lastModel.audiorepo name }, lastCmds, lastAudioCmds )

                            SOMAlert text ->
                                ( lastModel, lastCmds ++ [ config.ports.alert text ], lastAudioCmds )

                            SOMPrompt name title ->
                                ( lastModel, lastCmds ++ [ config.ports.prompt { name = name, title = title } ], lastAudioCmds )

                            SOMSaveUserData ->
                                let
                                    encodedGD =
                                        config.globalDataCodec.encode lastModel.currentGlobalData
                                in
                                ( lastModel, lastCmds ++ [ config.ports.sendInfo encodedGD ], lastAudioCmds )
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
update : UserConfig userdata scenemsg -> List ( String, SceneStorage userdata scenemsg ) -> AudioData -> WorldEvent -> Model userdata scenemsg -> ( Model userdata scenemsg, Cmd WorldEvent, AudioCmd WorldEvent )
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
                    maxHandW gd t

                ( fl, ft ) =
                    getStartPoint gd t

                oldIT =
                    gd.internalData

                newIT =
                    { oldIT | browserViewPort = t, realWidth = gw, realHeight = gh, startLeft = fl, startTop = ft }

                newgd =
                    { gd | internalData = newIT }
            in
            ( { model | currentGlobalData = newgd }, Cmd.none, Audio.cmdNone )

        WindowVisibility v ->
            ( { model | currentGlobalData = { gd | windowVisibility = v } }, Cmd.none, Audio.cmdNone )

        MouseMove ( px, py ) ->
            let
                mp =
                    fromMouseToVirtual gd ( px, py )
            in
            ( { model | currentGlobalData = { gd | mousePos = mp } }, Cmd.none, Audio.cmdNone )

        MouseDown e pos ->
            gameUpdate config scenes (MouseDown e <| fromMouseToVirtual model.currentGlobalData pos) model

        MouseUp e pos ->
            gameUpdate config scenes (MouseUp e <| fromMouseToVirtual model.currentGlobalData pos) model

        KeyDown 112 ->
            if config.debug then
                -- F1
                ( model, config.ports.prompt { name = "load", title = "Enter the scene you want to load" }, Audio.cmdNone )

            else
                gameUpdate config scenes msg model

        KeyDown 113 ->
            if config.debug then
                -- F2
                ( model, config.ports.prompt { name = "setVolume", title = "Set volume (0-1)" }, Audio.cmdNone )

            else
                gameUpdate config scenes msg model

        KeyUp key ->
            let
                newPressedKeys =
                    Set.remove key gd.pressedKeys
            in
            ( { model | currentGlobalData = { gd | pressedKeys = newPressedKeys } }, Cmd.none, Audio.cmdNone )

        KeyDown key ->
            let
                newPressedKeys =
                    Set.insert key gd.pressedKeys
            in
            ( { model | currentGlobalData = { gd | pressedKeys = newPressedKeys } }, Cmd.none, Audio.cmdNone )

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

        Tick x ->
            let
                newGD =
                    { gd | currentTimeStamp = x }

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
            gameUpdate config scenes msg { model | currentGlobalData = newGD, transition = newTrans }

        NullEvent ->
            ( model, Cmd.none, Audio.cmdNone )

        _ ->
            gameUpdate config scenes msg model
