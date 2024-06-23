module Messenger.UI.SOMHandler exposing (handleSOMs, handleSOM)

{-|


# Scene Output Message Handler

@docs handleSOMs, handleSOM

-}

import Audio exposing (AudioCmd)
import Messenger.Audio.Internal exposing (playAudio, stopAudio, updateAudio)
import Messenger.Base exposing (WorldEvent(..), globalDataToUserGlobalData)
import Messenger.GeneralModel exposing (filterSOM)
import Messenger.Model exposing (Model, resetSceneStartTime)
import Messenger.Recursion exposing (removeObjects, updateObjectsWithTarget)
import Messenger.Scene.Loader exposing (existScene, loadSceneByName)
import Messenger.Scene.Scene exposing (AllScenes, SceneOutputMsg(..))
import Messenger.UserConfig exposing (UserConfig)


{-| Handle a list of Scene Output Message.
-}
handleSOMs : UserConfig userdata scenemsg -> AllScenes userdata scenemsg -> List (SceneOutputMsg scenemsg userdata) -> Model userdata scenemsg -> ( Model userdata scenemsg, List (Cmd WorldEvent), List (AudioCmd WorldEvent) )
handleSOMs config scenes som model =
    let
        somhandle =
            handleSOM config scenes
    in
    List.foldl
        (\singleSOM ( lastModel, lastCmds, lastAudioCmds ) ->
            let
                ( newModel, newCmds, newAudioCmds ) =
                    somhandle singleSOM lastModel
            in
            ( newModel, newCmds ++ lastCmds, newAudioCmds ++ lastAudioCmds )
        )
        ( model, [], [] )
        som


{-| Handle a Scene Output Message.
-}
handleSOM : UserConfig userdata scenemsg -> AllScenes userdata scenemsg -> SceneOutputMsg scenemsg userdata -> Model userdata scenemsg -> ( Model userdata scenemsg, List (Cmd WorldEvent), List (AudioCmd WorldEvent) )
handleSOM config scenes som model =
    let
        gd =
            env.globalData

        env =
            model.env

        gdid =
            gd.internalData
    in
    case som of
        SOMChangeScene tm name ->
            -- Load new scene
            if existScene name scenes then
                ( loadSceneByName name scenes tm model
                    |> resetSceneStartTime
                , []
                , []
                )

            else
                ( model, [ config.ports.alert ("Scene" ++ name ++ "not found!") ], [] )

        SOMPlayAudio ch name opt ->
            let
                newRepo =
                    playAudio gdid.audioRepo ch name opt gd.currentTimeStamp

                newEnv =
                    { env | globalData = { gd | internalData = { gdid | audioRepo = newRepo } } }
            in
            ( { model | env = newEnv }, [], [] )

        SOMSetVolume s ->
            let
                newgd2 =
                    { gd | volume = s }

                newEnv =
                    { env | globalData = newgd2 }
            in
            ( { model | env = newEnv }, [], [] )

        SOMStopAudio ch ->
            let
                newRepo =
                    stopAudio gdid.audioRepo gd.currentTimeStamp ch

                newEnv =
                    { env | globalData = { gd | internalData = { gdid | audioRepo = newRepo } } }
            in
            ( { model | env = newEnv }, [], [] )

        SOMAlert text ->
            ( model, [ config.ports.alert text ], [] )

        SOMPrompt name title ->
            ( model, [ config.ports.prompt { name = name, title = title } ], [] )

        SOMSaveGlobalData ->
            let
                encodedGD =
                    config.globalDataCodec.encode (globalDataToUserGlobalData gd)
            in
            ( model, [ config.ports.sendInfo encodedGD ], [] )

        SOMLoadGC gc ->
            ( { model | globalComponents = model.globalComponents ++ [ gc env ] }, [], [] )

        SOMUnloadGC gctar ->
            ( { model | globalComponents = removeObjects gctar model.globalComponents }, [], [] )

        SOMCallGC call ->
            let
                ( gc1, som1, env1 ) =
                    updateObjectsWithTarget env [ call ] model.globalComponents

                model1 : Model userdata scenemsg
                model1 =
                    { globalComponents = gc1, env = env1 }
            in
            if List.isEmpty som1 then
                -- End
                ( model1, [], [] )

            else
                handleSOMs config scenes (filterSOM som1) model1

        SOMTransformAudio tar trans ->
            let
                newAR =
                    updateAudio gdid.audioRepo tar trans

                newModel =
                    { model | env = { env | globalData = { gd | internalData = { gdid | audioRepo = newAR } } } }
            in
            ( newModel, [], [] )
