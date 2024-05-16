module Messenger.Scene.Scene exposing
    ( AbstractScene(..)
    , MConcreteScene, MAbstractScene
    , unroll, abstract
    , SceneOutputMsg(..)
    , SceneStorage, AllScenes
    , SceneContext
    )

{-|


# Scene Base

Gerneral Model and Basic types for Scenes

@docs AbstractScene
@docs MConcreteScene, MAbstractScene
@docs unroll, abstract
@docs SceneOutputMsg
@docs SceneStorage, AllScenes
@docs SceneContext

-}

import Canvas exposing (Renderable)
import Messenger.Audio.Base exposing (AudioOption)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.Scene.Transitions.Base exposing (Transition)


{-| Concrete Scene Model

Users deal with the fields in concrete model.

-}
type alias ConcreteScene data env event ren scenemsg userdata =
    { init : env -> Maybe scenemsg -> data
    , update : env -> event -> data -> ( data, List (SceneOutputMsg scenemsg userdata), env )
    , view : env -> data -> ren
    }


{-| Unrolled Abstract Scene Model

The unrolled abstract model. Used internally.

-}
type alias UnrolledAbstractScene env event ren scenemsg userdata =
    { update : env -> event -> ( AbstractScene env event ren scenemsg userdata, List (SceneOutputMsg scenemsg userdata), env )
    , view : env -> ren
    }


{-| Rolled Abstract Scene Model.

Cannot be directedly modified.
Used for storage.

-}
type AbstractScene env event ren scenemsg userdata
    = Roll (UnrolledAbstractScene env event ren scenemsg userdata)


{-| Specialized Concrete scene for Messenger
-}
type alias MConcreteScene data userdata scenemsg =
    ConcreteScene data (Env () userdata) UserEvent Renderable scenemsg userdata


{-| Specialized Abstract scene for Messenger
-}
type alias MAbstractScene userdata scenemsg =
    AbstractScene (Env () userdata) UserEvent Renderable scenemsg userdata


{-| Unroll a rolled abstract scene.
-}
unroll : AbstractScene env event ren scenemsg userdata -> UnrolledAbstractScene env event ren scenemsg userdata
unroll (Roll un) =
    un


{-| Abstract a concrete scene to an abstract scene.

Initialize it with env and msg.

-}
abstract : ConcreteScene data env event ren scenemsg userdata -> Maybe scenemsg -> env -> AbstractScene env event ren scenemsg userdata
abstract conmodel initMsg initEnv =
    let
        abstractRec data =
            let
                updates : env -> event -> ( AbstractScene env event ren scenemsg userdata, List (SceneOutputMsg scenemsg userdata), env )
                updates env event =
                    let
                        ( new_d, new_m, new_e ) =
                            conmodel.update env event data
                    in
                    ( abstractRec new_d, new_m, new_e )

                views : env -> ren
                views env =
                    conmodel.view env data
            in
            Roll
                { update = updates
                , view = views
                }
    in
    abstractRec (conmodel.init initEnv initMsg)


{-| Scene Output Msg

Scene Output Msg is the Msg that directedly handled by Top-level.

`scenemsg` is a custom type which represents the message type users wants
to send to a scene when switching scenes.

  - `SOMChangeScene` is used to change to a target scene by giving a **(initMsg, name, trasition)**
  - `SOMPlayAudio` is used to play an audio resourse by giving **name url option**
  - `SOMStopAudio` is used to stop a playing audio by giving its **name**
  - `SOMSetVolume` is used to set the volume with a value **from 0 to 1**
  - `SOMAlert` makes an alert
  - `SOMPromp` makes a prompt with **name title**
  - `SOMSaveUserData` saves the userdata by encode funtion given in UserUonfig

-}
type SceneOutputMsg scenemsg userdata
    = SOMChangeScene (Maybe scenemsg) String (Maybe (Transition userdata))
    | SOMPlayAudio String String AudioOption -- audio name, audio url, audio option
    | SOMAlert String
    | SOMStopAudio String
    | SOMSetVolume Float
    | SOMPrompt String String -- name, title
    | SOMSaveGlobalData
    | SOMSetContext (SceneContext userdata scenemsg)
    | SOMGetContext (SceneContext userdata scenemsg -> userdata -> userdata)


{-| The type used to store the scene data.
-}
type alias SceneStorage userdata scenemsg =
    Maybe scenemsg -> Env () userdata -> MAbstractScene userdata scenemsg


{-| All scenes type
-}
type alias AllScenes userdata scenemsg =
    List ( String, SceneStorage userdata scenemsg )


{-| Scene Context
-}
type alias SceneContext userdata scenemsg =
    { scene : MAbstractScene userdata scenemsg
    , sceneStartTime : Int
    , name : String
    }
