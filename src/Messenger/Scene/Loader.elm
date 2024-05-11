module Messenger.Scene.Loader exposing
    ( SceneStorage
    , existScene, loadSceneByName
    )

{-|


# Scene Loader

Scene loader is used to find and load scenes.

@docs SceneStorage
@docs existScene, loadSceneByName

-}

import Messenger.Base exposing (Env)
import Messenger.Model exposing (Model)
import Messenger.Scene.Scene exposing (MAbstractScene)


{-| SceneStorage

The type used to store the scene data

-}
type alias SceneStorage userdata scenemsg =
    Env () userdata -> Maybe scenemsg -> MAbstractScene userdata scenemsg


{-| Query whether a scene exists
-}
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


{-| get a Scene from storage by name
-}
getScene : String -> List ( String, SceneStorage userdata scenemsg ) -> Maybe (SceneStorage userdata scenemsg)
getScene i scenes =
    List.head <|
        List.map (\( _, s ) -> s) <|
            List.filter (\( x, _ ) -> x == i) scenes


{-| load a Scene with init msg
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


{-| load a Scene from storage by name
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
