module Messenger.Component.PortableComponent exposing
    ( ConcretePortableComponent
    , PortableMsgCodec
    , PortableTarCodec
    , genPortableComponent
    , translatePortableComponent
    , PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
    , PortableComponentStorage
    )

{-|


## Portable components

These are components that might be provided by an elm package.

There are some limitations for portable components:

  - They don't have the base data
  - They cannot get the common data
  - They cannot change scene
  - You need to set the msg and target type for every dependent portable component
  - You need to provide a codec in the layer to translate the messages and targets

@docs ConcretePortableComponent
@docs PortableMsgCodec
@docs PortableTarCodec
@docs genPortableComponent
@docs translatePortableComponent


# Type sugar

@docs PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
@docs PortableComponentStorage

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, WorldEvent)
import Messenger.Component.Component exposing (AbstractComponent, ConcreteUserComponent)
import Messenger.GeneralModel exposing (Matcher, Msg(..), MsgBase(..), abstract)
import Messenger.Scene.Scene exposing (SceneOutputMsg)


{-| Portable component init type sugar
-}
type alias PortableComponentInit cdata userdata msg data =
    Env cdata userdata -> msg -> data


{-| Portable component update type sugar
-}
type alias PortableComponentUpdate cdata data userdata scenemsg tar msg =
    Env cdata userdata -> WorldEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), ( Env cdata userdata, Bool ) )


{-| Portable component updaterec type sugar
-}
type alias PortableComponentUpdateRec cdata data userdata scenemsg tar msg =
    Env cdata userdata -> msg -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )


{-| Portable component view type sugar
-}
type alias PortableComponentView cdata userdata data =
    Env cdata userdata -> data -> ( Renderable, Int )


{-| Portable component storage type sugar
-}
type alias PortableComponentStorage cdata userdata tar msg gtar gmsg bdata scenemsg =
    PortableMsgCodec msg gmsg -> PortableTarCodec tar gtar -> bdata -> Env cdata userdata -> gmsg -> AbstractComponent cdata userdata gtar gmsg bdata scenemsg


{-| ConcretePortableComponent

Used when createing a portable component.

Use `translatePortableComponent` to create a `ConcreteUserComponent` from a `ConcretePortableComponent`.

The `scenemsg` type is replaced by `()` because you cannot send changescene message.

-}
type alias ConcretePortableComponent data cdata userdata tar msg scenemsg =
    { init : PortableComponentInit cdata userdata msg data
    , update : PortableComponentUpdate cdata data userdata scenemsg tar msg
    , updaterec : PortableComponentUpdateRec cdata data userdata scenemsg tar msg
    , view : PortableComponentView cdata userdata data
    , matcher : Matcher data tar
    }


{-| Translate a `ConcretePortableComponent` to a `ConcreteUserComponent`.

This will add an empty basedata (unit) and upcast target and messages to the generalized type.

-}
translatePortableComponent : ConcretePortableComponent data cdata userdata tar msg scenemsg -> PortableMsgCodec msg gmsg -> PortableTarCodec tar gtar -> bdata -> ConcreteUserComponent data cdata userdata gtar gmsg bdata scenemsg
translatePortableComponent pcomp msgcodec tarcodec emptyBaseData =
    let
        msgMDecoder =
            genMsgDecoder msgcodec tarcodec
    in
    { init = \env gmsg -> ( pcomp.init env <| msgcodec.encode gmsg, emptyBaseData )
    , update =
        \env evt data baseData ->
            let
                ( resData, resMsg, resEnv ) =
                    pcomp.update env evt data
            in
            ( ( resData, baseData ), List.map msgMDecoder resMsg, resEnv )
    , updaterec =
        \env gmsg data baseData ->
            let
                ( resData, resMsg, resEnv ) =
                    pcomp.updaterec env (msgcodec.encode gmsg) data
            in
            ( ( resData, baseData ), List.map msgMDecoder resMsg, resEnv )
    , view = \env data _ -> pcomp.view env data
    , matcher = \data _ gtar -> pcomp.matcher data <| tarcodec.encode gtar
    }


{-| Msg decoder
-}
type alias MsgDecoder specifictar specificmsg generaltar generalmsg som =
    Msg specifictar specificmsg som -> Msg generaltar generalmsg som


{-| Portable Component Message Codec
-}
type alias PortableMsgCodec specificmsg generalmsg =
    { encode : generalmsg -> specificmsg
    , decode : specificmsg -> generalmsg
    }


{-| Portable Component Target Codec
-}
type alias PortableTarCodec specifictar generaltar =
    { encode : generaltar -> specifictar
    , decode : specifictar -> generaltar
    }


{-| Generate a message decoder
-}
genMsgDecoder : PortableMsgCodec specificmsg generalmsg -> PortableTarCodec specifictar generaltar -> MsgDecoder specifictar specificmsg generaltar generalmsg som
genMsgDecoder msgcodec tarcodec sMsgM =
    case sMsgM of
        Parent x ->
            case x of
                OtherMsg othermsg ->
                    Parent <| OtherMsg <| msgcodec.decode othermsg

                SOMMsg som ->
                    Parent <| SOMMsg som

        Other othertar smsg ->
            Other (tarcodec.decode othertar) (msgcodec.decode smsg)


{-| genPortableComponent

Generate abstract portable component from concrete component.

-}
genPortableComponent : ConcretePortableComponent data cdata userdata tar msg scenemsg -> PortableComponentStorage cdata userdata tar msg gtar gmsg bdata scenemsg
genPortableComponent conpcomp mcodec tcodec emptyBaseData env =
    abstract (translatePortableComponent conpcomp mcodec tcodec emptyBaseData) env
