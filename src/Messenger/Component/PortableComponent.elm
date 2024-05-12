module Messenger.Component.PortableComponent exposing
    ( ConcretePortableComponent
    , PortableMsgCodec
    , PortableTarCodec
    , genPortableComponentSpecific
    , translatePortableComponentSpecific
    , PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
    , PortableComponentStorageSpecific
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
@docs genPortableComponentSpecific
@docs translatePortableComponentSpecific


# Type sugar

@docs PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
@docs PortableComponentStorageSpecific

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, WorldEvent, addCommonData, removeCommonData)
import Messenger.Component.Component exposing (AbstractComponent, ConcreteUserComponent)
import Messenger.GeneralModel exposing (Matcher, Msg(..), MsgBase(..), abstract)
import Messenger.Scene.Scene exposing (SceneOutputMsg(..))


{-| Portable component init type sugar
-}
type alias PortableComponentInit userdata msg data =
    Env () userdata -> msg -> data


{-| Portable component update type sugar
-}
type alias PortableComponentUpdate data userdata tar msg =
    Env () userdata -> WorldEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), ( Env () userdata, Bool ) )


{-| Portable component updaterec type sugar
-}
type alias PortableComponentUpdateRec data userdata tar msg =
    Env () userdata -> msg -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), Env () userdata )


{-| Portable component view type sugar
-}
type alias PortableComponentView userdata data =
    Env () userdata -> data -> ( Renderable, Int )


{-| Portable component storage type sugar
-}
type alias PortableComponentStorageSpecific cdata userdata tar msg gtar gmsg bdata scenemsg =
    PortableMsgCodec msg gmsg -> PortableTarCodec tar gtar -> bdata -> scenemsg -> Env cdata userdata -> gmsg -> AbstractComponent cdata userdata gtar gmsg bdata scenemsg


{-| ConcretePortableComponent

Used when createing a portable component.

Use `translatePortableComponent` to create a `ConcreteUserComponent` from a `ConcretePortableComponent`.

The `scenemsg` type is replaced by `()` because you cannot send changescene message.

-}
type alias ConcretePortableComponent data userdata tar msg =
    { init : PortableComponentInit userdata msg data
    , update : PortableComponentUpdate data userdata tar msg
    , updaterec : PortableComponentUpdateRec data userdata tar msg
    , view : PortableComponentView userdata data
    , matcher : Matcher data tar
    }


{-| Translate a `ConcretePortableComponent` to a specific `ConcreteUserComponent`.
**Then you can treat it as a normal component in the declared type**

This will add an empty basedata (unit) when init and upcast target and messages to the generalized type.

You should also pass CommonData and SceneMsg with any value, which means you just need to match the data type.

-}
translatePortableComponentSpecific : ConcretePortableComponent data userdata tar msg -> PortableMsgCodec msg gmsg -> PortableTarCodec tar gtar -> bdata -> cdata -> scenemsg -> ConcreteUserComponent data cdata userdata gtar gmsg bdata scenemsg
translatePortableComponentSpecific pcomp msgcodec tarcodec emptyBaseData _ _ =
    let
        msgMDecoder =
            genMsgDecoder msgcodec tarcodec

        addSceneMsg : Msg gtar gmsg (SceneOutputMsg () userdata) -> Maybe (Msg gtar gmsg (SceneOutputMsg scenemsg userdata))
        addSceneMsg msg =
            case msg of
                Parent x ->
                    Maybe.map Parent <| addSceneMsgtoPortable x

                Other a b ->
                    Just <| Other a b
    in
    { init = \env gmsg -> ( pcomp.init (removeCommonData env) (msgcodec.encode gmsg), emptyBaseData )
    , update =
        \env evt data baseData ->
            let
                ( resData, resMsg, ( resEnv, resBlock ) ) =
                    pcomp.update (removeCommonData env) evt data
            in
            ( ( resData, baseData ), List.filterMap addSceneMsg <| List.map msgMDecoder resMsg, ( addCommonData env.commonData resEnv, resBlock ) )
    , updaterec =
        \env gmsg data baseData ->
            let
                ( resData, resMsg, resEnv ) =
                    pcomp.updaterec (removeCommonData env) (msgcodec.encode gmsg) data
            in
            ( ( resData, baseData ), List.filterMap addSceneMsg <| List.map msgMDecoder resMsg, addCommonData env.commonData resEnv )
    , view = \env data _ -> pcomp.view (removeCommonData env) data
    , matcher = \data _ gtar -> pcomp.matcher data <| tarcodec.encode gtar
    }


addSceneMsgtoPortable : MsgBase msg (SceneOutputMsg () userdata) -> Maybe (MsgBase msg (SceneOutputMsg scenemsg userdata))
addSceneMsgtoPortable msg =
    case msg of
        SOMMsg sommsg ->
            case sommsg of
                SOMChangeScene _ ->
                    Nothing

                SOMPlayAudio n u o ->
                    Just <| SOMMsg <| SOMPlayAudio n u o

                SOMAlert a ->
                    Just <| SOMMsg <| SOMAlert a

                SOMStopAudio n ->
                    Just <| SOMMsg <| SOMStopAudio n

                SOMSetVolume v ->
                    Just <| SOMMsg <| SOMSetVolume v

                SOMPrompt n t ->
                    Just <| SOMMsg <| SOMPrompt n t

                SOMSaveUserData ->
                    Just <| SOMMsg <| SOMSaveUserData

        OtherMsg othermsg ->
            Just <| OtherMsg othermsg


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
genPortableComponentSpecific : ConcretePortableComponent data userdata tar msg -> PortableComponentStorageSpecific cdata userdata tar msg gtar gmsg bdata scenemsg
genPortableComponentSpecific conpcomp mcodec tcodec emptyBaseData scenemsg env =
    abstract (translatePortableComponentSpecific conpcomp mcodec tcodec emptyBaseData env.commonData scenemsg) env
