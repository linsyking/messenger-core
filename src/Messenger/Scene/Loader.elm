module Messenger.Scene.Loader exposing (existScene, loadSceneByName)

{-|


# Scene Loader

Scene loader is used to find and load scenes.

@docs existScene, loadSceneByName

-}

import Dict
import Messenger.Base exposing (Env)
import Messenger.Model exposing (Model)
import Messenger.Scene.Scene exposing (AllScenes, SceneStorage)


{-| Query whether a scene exists.
-}
existScene : String -> AllScenes userdata scenemsg -> Bool
existScene i scenes =
    Dict.member i scenes


{-| Get a scene from storage by name.
-}
getScene : String -> AllScenes userdata scenemsg -> Maybe (SceneStorage userdata scenemsg)
getScene i scenes =
    Dict.get i scenes


{-| load a Scene with init msg
-}
loadScene : SceneStorage userdata scenemsg -> Maybe scenemsg -> Model userdata scenemsg -> Model userdata scenemsg
loadScene scenest smsg model =
    let
        env =
            Env model.currentGlobalData ()
    in
    { model | currentScene = scenest smsg env }


{-| load a Scene from storage by name
-}
loadSceneByName : String -> AllScenes userdata scenemsg -> Maybe scenemsg -> Model userdata scenemsg -> Model userdata scenemsg
loadSceneByName name scenes smsg model =
    case getScene name scenes of
        Just scenest ->
            let
                newModel =
                    loadScene scenest smsg model

                gd =
                    newModel.currentGlobalData
            in
            { newModel | currentGlobalData = { gd | currentScene = name } }

        Nothing ->
            model
