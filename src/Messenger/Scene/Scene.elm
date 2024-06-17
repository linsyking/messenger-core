module Messenger.Scene.Scene exposing
    ( AbstractScene(..)
    , MConcreteScene, MAbstractScene
    , unroll, abstract
    , SceneOutputMsg(..)
    , SceneStorage, AllScenes
    , MMsg, MMsgBase
    , MConcreteGeneralModel, MAbstractGeneralModel
    , GCCommonData, GCMsg, GCTarget
    , AbstractGlobalComponent, ConcreteGlobalComponent
    , GlobalComponentInit, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView
    , GlobalComponentStorage
    , genGlobalComponent
    )

{-|


# Scene Base

Gerneral Model and Basic types for Scenes

@docs AbstractScene
@docs MConcreteScene, MAbstractScene
@docs unroll, abstract
@docs SceneOutputMsg
@docs SceneStorage, AllScenes
@docs MMsg, MMsgBase
@docs MConcreteGeneralModel, MAbstractGeneralModel


# Global Component

@docs GCCommonData, GCMsg, GCTarget
@docs AbstractGlobalComponent, ConcreteGlobalComponent
@docs GlobalComponentInit, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView
@docs GlobalComponentStorage

@docs genGlobalComponent

-}

import Canvas exposing (Renderable)
import Json.Decode
import Messenger.Audio.Base exposing (AudioOption)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.GeneralModel as GM exposing (AbstractGeneralModel, ConcreteGeneralModel, Msg, MsgBase)


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


{-| Scene Output Msg is the message that directedly handled by the top-level core.

`scenemsg` is a custom type which represents the message type users wants
to send to a scene when switching scenes.

  - `SOMChangeScene` is used to change to a target scene by giving a **initMsg name**
  - `SOMPlayAudio channel name option` is used to play an audio resource by giving **channel name option**
  - `SOMStopAudio` is used to stop a playing audio by giving its **name**
  - `SOMSetVolume` is used to set the volume with a value **from 0 to 1**
  - `SOMAlert` makes an alert
  - `SOMPromp name title` makes a prompt with **name title**
  - `SOMSaveGlobalData` saves the global by encode funtion given in UserConfig

-}
type SceneOutputMsg scenemsg userdata
    = SOMChangeScene (Maybe scenemsg) String
    | SOMAlert String
    | SOMPrompt String String
    | SOMPlayAudio Int String AudioOption
    | SOMStopAudio Int
    | SOMSetVolume Float
    | SOMSaveGlobalData
    | SOMLoadGC (GlobalComponentStorage userdata scenemsg)
    | SOMUnloadGC GCTarget
    | SOMCallGC GCTarget GCMsg


{-| The type used to store the scene data.
-}
type alias SceneStorage userdata scenemsg =
    Maybe scenemsg -> Env () userdata -> MAbstractScene userdata scenemsg


{-| All scenes type
-}
type alias AllScenes userdata scenemsg =
    List ( String, SceneStorage userdata scenemsg )


{-| Messsenger MsgBase
-}
type alias MMsgBase othermsg scenemsg userdata =
    MsgBase othermsg (SceneOutputMsg scenemsg userdata)


{-| Messenger Msg
-}
type alias MMsg othertar msg scenemsg userdata =
    Msg othertar msg (SceneOutputMsg scenemsg userdata)


{-| Specialized Concrete Model for Messenger
-}
type alias MConcreteGeneralModel data common userdata tar msg bdata scenemsg =
    ConcreteGeneralModel data (Env common userdata) UserEvent tar msg Renderable bdata (SceneOutputMsg scenemsg userdata)


{-| Specialized Abstract Model for Messenger
-}
type alias MAbstractGeneralModel common userdata tar msg bdata scenemsg =
    AbstractGeneralModel (Env common userdata) UserEvent tar msg Renderable bdata (SceneOutputMsg scenemsg userdata)



--- Global Component


type alias GCCommonData userdata scenemsg =
    MAbstractScene userdata scenemsg


{-| Global component message type.
-}
type alias GCMsg =
    Json.Decode.Value


{-| Global component target type.
-}
type alias GCTarget =
    String


{-| init type sugar
-}
type alias GlobalComponentInit userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> GCMsg -> data


{-| update type sugar
-}
type alias GlobalComponentUpdate userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> UserEvent -> data -> ( data, List (MMsg GCTarget GCMsg scenemsg userdata), ( Env (GCCommonData userdata scenemsg) userdata, Bool ) )


{-| updaterec type sugar
-}
type alias GlobalComponentUpdateRec userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> GCMsg -> data -> ( data, List (MMsg GCTarget GCMsg scenemsg userdata), Env (GCCommonData userdata scenemsg) userdata )


{-| view type sugar
-}
type alias GlobalComponentView userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> data -> Renderable


{-| GlobalComponent Storage
-}
type alias GlobalComponentStorage userdata scenemsg =
    Env (GCCommonData userdata scenemsg) userdata -> AbstractGlobalComponent userdata scenemsg


{-| Concrete GlobalComponent Model
-}
type alias ConcreteGlobalComponent data userdata scenemsg =
    { init : GlobalComponentInit userdata scenemsg data
    , update : GlobalComponentUpdate userdata scenemsg data
    , updaterec : GlobalComponentUpdateRec userdata scenemsg data
    , view : GlobalComponentView userdata scenemsg data
    , matcher : GCTarget
    }


{-| Abstract GlobalComponent Model.

Cannot be directedly modified.
Used for storage.

-}
type alias AbstractGlobalComponent userdata scenemsg =
    MAbstractGeneralModel (GCCommonData userdata scenemsg) userdata GCTarget GCMsg () scenemsg


{-| Generate abstract global component from concrete global component.
-}
genGlobalComponent : ConcreteGlobalComponent data userdata scenemsg -> GCMsg -> Maybe GCTarget -> GlobalComponentStorage userdata scenemsg
genGlobalComponent conpcomp gcmsg gctar =
    GM.abstract (addEmptyBData conpcomp gctar) <| gcmsg


{-| Turn global component into a general model.
-}
addEmptyBData : ConcreteGlobalComponent data userdata scenemsg -> Maybe GCTarget -> MConcreteGeneralModel data (GCCommonData userdata scenemsg) userdata GCTarget GCMsg () scenemsg
addEmptyBData mconnoB gctar =
    let
        id =
            case gctar of
                Just t ->
                    t

                Nothing ->
                    mconnoB.matcher
    in
    { init = \env msg -> ( mconnoB.init env msg, () )
    , update =
        \env evt data () ->
            let
                ( resData, resMsg, resEnv ) =
                    mconnoB.update env evt data
            in
            ( ( resData, () ), resMsg, resEnv )
    , updaterec =
        \env msg data () ->
            let
                ( resData, resMsg, resEnv ) =
                    mconnoB.updaterec env msg data
            in
            ( ( resData, () ), resMsg, resEnv )
    , view = \env data () -> mconnoB.view env data
    , matcher = \_ _ tar -> tar == id
    }
