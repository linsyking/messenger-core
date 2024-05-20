module Messenger.UI.SOMHandler exposing (handleSOM)

{-|


# Scene Output Message Handler

@docs handleSOM

-}

import Audio exposing (AudioCmd)
import Messenger.Audio.Audio exposing (stopAudio)
import Messenger.Base exposing (WorldEvent(..), globalDataToUserGlobalData)
import Messenger.Model exposing (Model, resetSceneStartTime)
import Messenger.Scene.Loader exposing (existScene, loadSceneByName)
import Messenger.Scene.Scene exposing (AllScenes, SceneOutputMsg(..))
import Messenger.UserConfig exposing (UserConfig)


{-| Handle a Scene Output Message.
-}
handleSOM : UserConfig userdata scenemsg -> AllScenes userdata scenemsg -> SceneOutputMsg scenemsg userdata -> Model userdata scenemsg -> ( Model userdata scenemsg, List (Cmd WorldEvent), List (AudioCmd WorldEvent) )
handleSOM config scenes som model =
    case som of
        SOMChangeScene tm name ptrans ->
            if model.transition == Nothing then
                case ptrans of
                    Just trans ->
                        -- Delayed Loading
                        if existScene name scenes then
                            ( { model | transition = Just ( trans, ( name, tm ) ) }
                            , []
                            , []
                            )

                        else
                            ( model, [ config.ports.alert ("Scene" ++ name ++ "not found!") ], [] )

                    Nothing ->
                        -- Load new scene
                        if existScene name scenes then
                            ( loadSceneByName name scenes tm model
                                |> resetSceneStartTime
                            , []
                            , []
                            )

                        else
                            ( model, [ config.ports.alert ("Scene" ++ name ++ "not found!") ], [] )

            else
                -- In transition
                ( model, [], [] )

        SOMPlayAudio name path opt ->
            ( model, [], [ Audio.loadAudio (SoundLoaded name opt) path ] )

        SOMSetVolume s ->
            let
                oldgd =
                    model.currentGlobalData

                newgd2 =
                    { oldgd | volume = s }
            in
            ( { model | currentGlobalData = newgd2 }, [], [] )

        SOMStopAudio name ->
            ( { model | audiorepo = stopAudio model.audiorepo name }, [], [] )

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

        SOMSetContext ctx ->
            let
                oldgd =
                    model.currentGlobalData

                newgd =
                    { oldgd | sceneStartTime = ctx.sceneStartTime, currentScene = ctx.name }

                newModel =
                    { model | currentGlobalData = newgd, currentScene = ctx.scene }
            in
            ( newModel, [], [] )

        SOMGetContext getter ->
            let
                ctx =
                    { scene = model.currentScene, sceneStartTime = oldgd.sceneStartTime, name = oldgd.currentScene }

                oldgd =
                    model.currentGlobalData

                newgd =
                    { oldgd | userData = getter ctx oldgd.userData }
            in
            ( { model | currentGlobalData = newgd }, [], [] )
