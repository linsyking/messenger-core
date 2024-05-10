module Messenger.Scene.Loader exposing (..)

{-| Query whether a scene exists
-}

import Messenger.Base exposing (Env)
import Messenger.Model exposing (Model)
import Messenger.Scene.Scene exposing (MAbstractScene)


type alias SceneStorage userdata scenemsg =
    Env () userdata -> Maybe scenemsg -> MAbstractScene userdata scenemsg


existScene : String -> List ( String, SceneStorage userdata scenemsg ) -> Bool
existScene i scenes =
    let
        tests =
            List.filter (\( x, _ ) -> x == i) scenes
    in
    case List.head tests of
        Just _ ->
            True

        Nothing ->
            False


{-| getScene
-}
getScene : String -> List ( String, SceneStorage userdata scenemsg ) -> Maybe (SceneStorage userdata scenemsg)
getScene i scenes =
    List.head <|
        List.map (\( _, s ) -> s) <|
            List.filter (\( x, _ ) -> x == i) scenes


{-| loadScene
-}
loadScene : Maybe (SceneStorage userdata scenemsg) -> Maybe scenemsg -> Model userdata scenemsg -> Model userdata scenemsg
loadScene scenest smsg model =
    case scenest of
        Just s ->
            let
                env =
                    Env model.currentGlobalData ()
            in
            { model | currentScene = s env smsg }

        Nothing ->
            model


{-| loadSceneByName
-}
loadSceneByName : String -> List ( String, SceneStorage userdata scenemsg ) -> Maybe scenemsg -> Model userdata scenemsg -> Model userdata scenemsg
loadSceneByName name scenes smsg model =
    let
        newModel =
            loadScene (getScene name scenes) smsg model

        gd =
            newModel.currentGlobalData
    in
    { newModel | currentGlobalData = { gd | currentScene = name } }
