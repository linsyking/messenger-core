module Messenger.Component.PortableComponent exposing
    ( ConcretePortableComponent
    , PortableMsgCodec
    , PortableTarCodec
    , genPortableComponentSpecific
    , translatePortableComponentSpecific
    , AbstractGeneralPortableComponent
    , genPortableComponentGeneral
    , translatePortableComponentGeneral
    , updatePortableComponents, updatePortableComponentsWithTarget
    , PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
    , PortableComponentStorageSpecific, PortableComponentStorageGeneral
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

A concrete portable component can generate two kinds of abstract component: **Specific Component** or **General Portable Component**.

@docs ConcretePortableComponent
@docs PortableMsgCodec
@docs PortableTarCodec


### Specific Component

You can generate a general type of component from a concrete portable component, which means you can manage this component with a
list of user components.

@docs genPortableComponentSpecific
@docs translatePortableComponentSpecific


### General Portable Component

You can generate a component in specific type from a concrete portable component, which means you can manage this component in a
list for all general portable components generated in this type.

**You should update this type of components by using specialized function provided.**

@docs AbstractGeneralPortableComponent
@docs genPortableComponentGeneral
@docs translatePortableComponentGeneral
@docs updatePortableComponents, updatePortableComponentsWithTarget


# Type sugar

@docs PortableComponentInit, PortableComponentUpdate, PortableComponentUpdateRec, PortableComponentView
@docs PortableComponentStorageSpecific, PortableComponentStorageGeneral

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, UserEvent, addCommonData, removeCommonData)
import Messenger.Component.Component exposing (AbstractComponent, ConcreteUserComponent)
import Messenger.GeneralModel exposing (Matcher, Msg(..), MsgBase(..), abstract)
import Messenger.Recursion exposing (updateObjects, updateObjectsWithTarget)
import Messenger.Scene.Scene exposing (SceneOutputMsg(..))


{-| Portable component init type sugar
-}
type alias PortableComponentInit userdata msg data =
    Env () userdata -> msg -> data


{-| Portable component update type sugar
-}
type alias PortableComponentUpdate data userdata tar msg =
    Env () userdata -> UserEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), ( Env () userdata, Bool ) )


{-| Portable component updaterec type sugar
-}
type alias PortableComponentUpdateRec data userdata tar msg =
    Env () userdata -> msg -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), Env () userdata )


{-| Portable component view type sugar
-}
type alias PortableComponentView userdata data =
    Env () userdata -> data -> ( Renderable, Int )


{-| Portable component storage as a specific component type sugar
-}
type alias PortableComponentStorageSpecific cdata userdata tar msg gtar gmsg bdata scenemsg =
    PortableTarCodec tar gtar -> PortableMsgCodec msg gmsg -> bdata -> Env cdata userdata -> gmsg -> AbstractComponent cdata userdata gtar gmsg bdata scenemsg


{-| Portable component storage as general portable component type sugar
-}
type alias PortableComponentStorageGeneral cdata userdata tar msg gtar gmsg =
    PortableTarCodec tar gtar -> PortableMsgCodec msg gmsg -> Env cdata userdata -> gmsg -> AbstractGeneralPortableComponent userdata gtar gmsg


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


{-| AbstractPortableComponent

Abstract General Portable Component with common data, base data, and scene msg set to unit type.

This means you cannot send scene msg from a portable component. And you cannot put a general portable
component in a list of other user components.

-}
type alias AbstractGeneralPortableComponent userdata tar msg =
    AbstractComponent () userdata tar msg () ()


{-| Translate a `ConcretePortableComponent` to a specific `ConcreteUserComponent`.
**Then you can treat it as a normal component in the declared type**

This will add an empty basedata provided when init and upcast target and messages to the generalized type.

You should also pass CommonData and SceneMsg with any value, which means you just need to match the data type.

-}
translatePortableComponentSpecific : ConcretePortableComponent data userdata tar msg -> PortableTarCodec tar gtar -> PortableMsgCodec msg gmsg -> bdata -> ConcreteUserComponent data cdata userdata gtar gmsg bdata scenemsg
translatePortableComponentSpecific pcomp tarcodec msgcodec emptyBaseData =
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


{-| Translate a `ConcretePortableComponent` to a special kind of `ConcreteUserComponent`.
**Then you should treat it as a general portable component, in the same type of other general portable components **

This will add an empty basedata (unit) when init and upcast target and messages to the generalized type.

The commondata and scenemsg will also be set to unit type.

-}
translatePortableComponentGeneral : ConcretePortableComponent data userdata tar msg -> PortableTarCodec tar gtar -> PortableMsgCodec msg gmsg -> ConcreteUserComponent data () userdata gtar gmsg () ()
translatePortableComponentGeneral pcomp tarcodec msgcodec =
    translatePortableComponentSpecific pcomp tarcodec msgcodec ()


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

                SOMSaveGlobalData ->
                    Just <| SOMMsg <| SOMSaveGlobalData

                SOMSetContext _ ->
                    Nothing

                SOMGetContext _ ->
                    Nothing

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



--- Generator


{-| genPortableComponentSpecific

Generate abstract component in a specific component type from concrete component.

-}
genPortableComponentSpecific : ConcretePortableComponent data userdata tar msg -> PortableComponentStorageSpecific cdata userdata tar msg gtar gmsg bdata scenemsg
genPortableComponentSpecific conpcomp tcodec mcodec emptyBaseData env =
    abstract (translatePortableComponentSpecific conpcomp tcodec mcodec emptyBaseData) env


{-| genPortableComponentGeneral

Generate abstract component in general portable type from concrete component.

**you don't need to give a Env without commondata**

-}
genPortableComponentGeneral : ConcretePortableComponent data userdata tar msg -> PortableComponentStorageGeneral cdata userdata tar msg gtar gmsg
genPortableComponentGeneral conpcomp tcodec mcodec env =
    abstract (translatePortableComponentGeneral conpcomp tcodec mcodec) <| removeCommonData env



-- Updater for General Portable Components


{-| updatePortableComponents

Update a list of abstract general portable components.

**Use this function instead of updateComponents to update general portable components!**

**you don't need to give a Env without commondata**

-}
updatePortableComponents : Env cdata userdata -> UserEvent -> List (AbstractGeneralPortableComponent userdata tar msg) -> ( List (AbstractGeneralPortableComponent userdata tar msg), List (MsgBase msg (SceneOutputMsg scenemsg userdata)), ( Env cdata userdata, Bool ) )
updatePortableComponents env evt pcomps =
    let
        ( newpcomps, newMsg, ( newEnv, newBlock ) ) =
            updateObjects (removeCommonData env) evt pcomps

        newEnvC =
            addCommonData env.commonData newEnv

        newMsgfilterd =
            List.filterMap addSceneMsgtoPortable newMsg
    in
    ( newpcomps, newMsgfilterd, ( newEnvC, newBlock ) )


{-| updatePortableComponentsWithTarget

Update a list of abstract portable components with target msgs.

**Use this function instead of updateComponentsWithTarget to update general portable components!**

**you don't need to give a Env without commondata**

-}
updatePortableComponentsWithTarget : Env cdata userdata -> List (Msg tar msg (SceneOutputMsg () userdata)) -> List (AbstractGeneralPortableComponent userdata tar msg) -> ( List (AbstractGeneralPortableComponent userdata tar msg), List (MsgBase msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )
updatePortableComponentsWithTarget env msgs pcomps =
    let
        ( newpcomps, newMsg, newEnv ) =
            updateObjectsWithTarget (removeCommonData env) msgs pcomps

        newEnvC =
            addCommonData env.commonData newEnv

        newMsgfilterd =
            List.filterMap addSceneMsgtoPortable newMsg
    in
    ( newpcomps, newMsgfilterd, newEnvC )
