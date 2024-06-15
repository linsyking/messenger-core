module Messenger.Component.PortableComponent exposing
    ( ConcretePortableComponent
    , PortableMsgCodec
    , PortableTarCodec
    , genPortableComponent
    , PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
    , PortableComponentStorage
    )

{-|


# Portable components

These are components that might be provided by an elm package.

There are some limitations for portable components:

  - They don't have the base data
  - They cannot get the common data
  - You need to set the msg and target type for every portable component
  - You need to provide a codec in the layer to translate the messages and targets

@docs ConcretePortableComponent
@docs PortableMsgCodec
@docs PortableTarCodec

You can generate a general type of component from a concrete portable component, which means you can manage this component with a
list of user components.

@docs genPortableComponent


## Type sugar

@docs PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
@docs PortableComponentStorage

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, UserEvent, addCommonData, removeCommonData)
import Messenger.Component.Component exposing (AbstractComponent, ConcreteUserComponent)
import Messenger.GeneralModel exposing (MMsg, Matcher, Msg(..), MsgBase(..), abstract)
import Messenger.Scene.Scene exposing (SceneOutputMsg(..))


{-| Portable component init type sugar
-}
type alias PortableComponentInit userdata msg data =
    Env () userdata -> msg -> data


{-| Portable component update type sugar
-}
type alias PortableComponentUpdate data userdata tar msg scenemsg =
    Env () userdata -> UserEvent -> data -> ( data, List (MMsg tar msg scenemsg userdata), ( Env () userdata, Bool ) )


{-| Portable component updaterec type sugar
-}
type alias PortableComponentUpdateRec data userdata tar msg scenemsg =
    Env () userdata -> msg -> data -> ( data, List (MMsg tar msg scenemsg userdata), Env () userdata )


{-| Portable component view type sugar
-}
type alias PortableComponentView userdata data =
    Env () userdata -> data -> Renderable


{-| Portable component storage as a specific component type sugar
-}
type alias PortableComponentStorage cdata userdata gtar gmsg bdata scenemsg =
    gmsg -> Env cdata userdata -> AbstractComponent cdata userdata gtar gmsg bdata scenemsg


{-| ConcretePortableComponent

Used when createing a portable component.

Use `translatePortableComponent` to create a `ConcreteUserComponent` from a `ConcretePortableComponent`.

The `scenemsg` type is replaced by `()` because you cannot send changescene message.

-}
type alias ConcretePortableComponent data userdata tar msg scenemsg =
    { init : PortableComponentInit userdata msg data
    , update : PortableComponentUpdate data userdata tar msg scenemsg
    , updaterec : PortableComponentUpdateRec data userdata tar msg scenemsg
    , view : PortableComponentView userdata data
    , matcher : Matcher data tar
    }


{-| Translate a `ConcretePortableComponent` to a specific `ConcreteUserComponent`.

**Then you can treat it as a normal component in the declared type**

This will add an empty basedata provided when init and upcast target and messages to the generalized type.

You should pass a BaseData with any value, which means you just need to match the data type.

-}
translatePortableComponent : ConcretePortableComponent data userdata tar msg scenemsg -> PortableTarCodec tar gtar -> PortableMsgCodec msg gmsg -> bdata -> Int -> ConcreteUserComponent data cdata userdata gtar gmsg bdata scenemsg
translatePortableComponent pcomp tarcodec msgcodec emptyBaseData zindex =
    let
        msgMDecoder =
            genMsgDecoder msgcodec tarcodec
    in
    { init = \env gmsg -> ( pcomp.init (removeCommonData env) (msgcodec.encode gmsg), emptyBaseData )
    , update =
        \env evt data baseData ->
            let
                ( resData, resMsg, ( resEnv, resBlock ) ) =
                    pcomp.update (removeCommonData env) evt data
            in
            ( ( resData, baseData ), List.map msgMDecoder resMsg, ( addCommonData env.commonData resEnv, resBlock ) )
    , updaterec =
        \env gmsg data baseData ->
            let
                ( resData, resMsg, resEnv ) =
                    pcomp.updaterec (removeCommonData env) (msgcodec.encode gmsg) data
            in
            ( ( resData, baseData ), List.map msgMDecoder resMsg, addCommonData env.commonData resEnv )
    , view = \env data _ -> ( pcomp.view (removeCommonData env) data, zindex )
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


{-| Generate a message decoder.
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

        Other ( othertar, smsg ) ->
            Other ( tarcodec.decode othertar, msgcodec.decode smsg )


{-| Generate abstract component from concrete component.
-}
genPortableComponent : ConcretePortableComponent data userdata tar msg scenemsg -> PortableTarCodec tar gtar -> PortableMsgCodec msg gmsg -> bdata -> Int -> PortableComponentStorage cdata userdata gtar gmsg bdata scenemsg
genPortableComponent conpcomp tcodec mcodec emptyBaseData zindex =
    abstract (translatePortableComponent conpcomp tcodec mcodec emptyBaseData zindex)
