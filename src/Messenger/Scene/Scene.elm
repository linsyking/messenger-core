module Messenger.Scene.Scene exposing
    ( AbstractScene(..)
    , MConcreteScene, MAbstractScene
    , unroll, abstract
    , SceneOutputMsg(..)
    , SceneStorage, AllScenes
    , MMsg, MMsgBase
    , MConcreteGeneralModel, MAbstractGeneralModel
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

-}

import Canvas exposing (Renderable)
import Json.Decode
import Messenger.Audio.Base exposing (AudioOption)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.GeneralModel exposing (AbstractGeneralModel, ConcreteGeneralModel, Msg, MsgBase)


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

  - `SOMChangeScene` is used to change to a target scene by giving a **initMsg name trasition**
  - `SOMPlayAudio channel name option` is used to play an audio resource by giving **channel name option**
  - `SOMStopAudio` is used to stop a playing audio by giving its **name**
  - `SOMSetVolume` is used to set the volume with a value **from 0 to 1**
  - `SOMAlert` makes an alert
  - `SOMPromp name title` makes a prompt with **name title**
  - `SOMSaveGlobalData` saves the global by encode funtion given in UserConfig
  - `SOMSetContext` restores the context of the scene
  - `SOMGetContext` gets the context of the scene

-}
type SceneOutputMsg scenemsg userdata
    = SOMChangeScene (Maybe scenemsg) String
    | SOMAlert String
    | SOMPrompt String String
    | SOMPlayAudio Int String AudioOption
    | SOMStopAudio Int
    | SOMSetVolume Float
    | SOMSaveGlobalData


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
type alias GlobalComponentInit userdata data =
    Env (GCCommonData userdata scenemsg) userdata -> GCMsg -> data


{-| update type sugar
-}
type alias GlobalComponentUpdate cdata userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> UserEvent -> data -> ( data, List (MMsg GCTarget GCMsg scenemsg userdata), ( Env (GCCommonData userdata scenemsg) userdata, Bool ) )


{-| updaterec type sugar
-}
type alias GlobalComponentUpdateRec userdata tar msg scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> msg -> data -> ( data, List (MMsg tar msg scenemsg userdata), Env (GCCommonData userdata scenemsg) userdata )


{-| view type sugar
-}
type alias GlobalComponentView cdata userdata data =
    Env cdata userdata -> data -> Renderable


{-| GlobalComponent Storage
-}
type alias GlobalComponentStorage userdata scenemsg =
    Env () userdata -> AbstractGlobalComponent cdata userdata tar msg scenemsg


{-| Concrete GlobalComponent Model

Users deal with the fields in concrete model.

-}
type alias ConcreteGlobalComponent data cdata userdata tar msg scenemsg =
    { init : GlobalComponentInit cdata userdata msg data
    , update : GlobalComponentUpdate cdata userdata tar msg scenemsg data
    , updaterec : GlobalComponentUpdateRec cdata userdata tar msg scenemsg data
    , view : GlobalComponentView cdata userdata data
    , matcher : Matcher data tar
    }


{-| Abstract GlobalComponent Model.

Cannot be directedly modified.
Used for storage.

-}
type alias AbstractGlobalComponent cdata userdata tar msg scenemsg =
    MAbstractGeneralModel cdata userdata tar msg () scenemsg


{-| Global component storage type.
-}
type alias GlobalComponentStorage cdata userdata scenemsg =
    Env cdata userdata -> AbstractGlobalComponent cdata userdata GCTarget GCMsg scenemsg


{-| Translate a `ConcreteGlobalComponent` to a specific `ConcreteGlobalComponent`.
-}
translateGlobalComponent : ConcreteGlobalComponent data cdata userdata tar msg scenemsg -> GlobalTarCodec tar -> GlobalMsgCodec msg -> ConcreteGlobalComponent data cdata userdata GCTarget GCMsg scenemsg
translateGlobalComponent pcomp tarcodec msgcodec =
    let
        msgMDecoder =
            genMsgDecoder msgcodec tarcodec
    in
    { init = \env gmsg -> pcomp.init env (msgcodec.encode gmsg)
    , update =
        \env evt data ->
            let
                ( resData, resMsg, ( resEnv, resBlock ) ) =
                    pcomp.update env evt data
            in
            ( resData, List.map msgMDecoder resMsg, ( resEnv, resBlock ) )
    , updaterec =
        \env gmsg data ->
            let
                ( resData, resMsg, resEnv ) =
                    pcomp.updaterec env (msgcodec.encode gmsg) data
            in
            ( resData, List.map msgMDecoder resMsg, resEnv )
    , view = \env data -> pcomp.view env data
    , matcher = \data gtar -> pcomp.matcher data <| tarcodec.encode gtar
    }


{-| Msg decoder
-}
type alias MsgDecoder specifictar specificmsg som =
    Msg specifictar specificmsg som -> Msg GCTarget GCMsg som


{-| Global Component Message Codec
-}
type alias GlobalMsgCodec specificmsg =
    { encode : GCMsg -> specificmsg
    , decode : specificmsg -> GCMsg
    }


{-| Global Component Target Codec
-}
type alias GlobalTarCodec specifictar =
    { encode : GCTarget -> specifictar
    , decode : specifictar -> GCTarget
    }


{-| Generate a message decoder.
-}
genMsgDecoder : GlobalMsgCodec specificmsg -> GlobalTarCodec specifictar -> MsgDecoder specifictar specificmsg som
genMsgDecoder msgcodec tarcodec sMsgM =
    case sMsgM of
        Parent x ->
            case x of
                OtherMsg othermsg ->
                    Parent <| OtherMsg <| msgcodec.decode othermsg

                SOMMsg som ->
                    Parent <| SOMMsg som

        Other ( othertar, smsg ) ->
            Other ( tarcodec.decode othertar, msgcodec.decode smsg )


{-| Generate abstract global component from concrete global component.
-}
genGlobalComponent : ConcreteGlobalComponent data cdata userdata tar msg scenemsg -> GlobalTarCodec tar -> GlobalMsgCodec msg -> GCMsg -> GlobalComponentStorage cdata userdata scenemsg
genGlobalComponent conpcomp tcodec mcodec gcmsg =
    genLayer (translateGlobalComponent conpcomp tcodec mcodec) gcmsg
