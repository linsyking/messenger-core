module Messenger.UI.SOMHandler exposing (handleSOM)

{-|


# Scene Output Message Handler

@docs handleSOM

-}

import Audio exposing (AudioCmd)
import Messenger.Audio.Internal exposing (playAudio, stopAudio)
import Messenger.Base exposing (WorldEvent(..), globalDataToUserGlobalData)
import Messenger.Model exposing (Model, resetSceneStartTime)
import Messenger.Scene.Loader exposing (existScene, loadSceneByName)
import Messenger.Scene.Scene exposing (AllScenes, SceneOutputMsg(..))
import Messenger.UserConfig exposing (UserConfig)


{-| Handle a Scene Output Message.
-}
handleSOM : UserConfig userdata scenemsg -> AllScenes userdata scenemsg -> SceneOutputMsg scenemsg userdata -> Model userdata scenemsg -> ( Model userdata scenemsg, List (Cmd WorldEvent), List (AudioCmd WorldEvent) )
handleSOM config scenes som model =
    let
        gd =
            model.currentGlobalData

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
            in
            ( { model | currentGlobalData = { gd | internalData = { gdid | audioRepo = newRepo } } }, [], [] )

        SOMSetVolume s ->
            let
                newgd2 =
                    { gd | volume = s }
            in
            ( { model | currentGlobalData = newgd2 }, [], [] )

        SOMStopAudio ch ->
            let
                newRepo =
                    stopAudio gdid.audioRepo gd.currentTimeStamp ch
            in
            ( { model | currentGlobalData = { gd | internalData = { gdid | audioRepo = newRepo } } }, [], [] )

        SOMAlert text ->
            ( model, [ config.ports.alert text ], [] )

        SOMPrompt name title ->
            ( model, [ config.ports.prompt { name = name, title = title } ], [] )

        SOMSaveGlobalData ->
            let
                encodedGD =
                    config.globalDataCodec.encode (globalDataToUserGlobalData model.currentGlobalData)
            in
            ( model, [ config.ports.sendInfo encodedGD ], [] )
