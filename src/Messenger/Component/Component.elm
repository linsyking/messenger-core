module Messenger.Component.Component exposing
    ( AbstractComponent
    , AbstractPortableComponent
    , ConcretePortableComponent
    , ConcreteUserComponent
    , PortableMsgCodec
    , PortableTarCodec
    , genComponent
    , genComponentsRenderList
    , translatePortableComponent
    , updateComponents
    , updateComponentsWithTarget
    , updatePortableComponents
    , updatePortableComponentsWithTarget
    , viewComponents
    , viewComponentsRenderList
    )

{-|


# Component

A component is an object that you may put in your layers.

There are two types of components:

  - Portable components
  - User components


## Portable components

These are components that might be provided by an elm package.

There are some limitations for portable components:

  - They cannot change scene
  - You need to use a codec to translate the messages and targets


## User components

These are components that you users can create with custom **basedata**.

Basedata is the data that components with the same type can share.

For example, you may want a game component to have some common properties like position, velocity, etc.

In this case, your basedata would be a record with these properties.

-}

import Canvas exposing (Renderable, group)
import Messenger.Base exposing (Env, WorldEvent)
import Messenger.GeneralModel exposing (AbstractGeneralModel, ConcreteGeneralModel, Msg(..), MsgBase(..), abstract, unroll)
import Messenger.Recursion exposing (updateObjects, updateObjectsWithTarget)
import Messenger.Scene.Scene exposing (SceneOutputMsg(..), addCommonData, noCommonData)


{-| ConcreteUserComponent
-}
type alias ConcreteUserComponent data cdata userdata tar msg bdata scenemsg =
    ConcreteGeneralModel data (Env cdata userdata) WorldEvent tar msg ( Renderable, Int ) bdata (SceneOutputMsg scenemsg userdata)


{-| ConcretePortableComponent

Used when createing a portable component.

Use `translatePortableComponent` to create a `ConcreteUserComponent` from a `ConcretePortableComponent`.

The `scenemsg` type is replaced by `()` because you cannot send changescene message.

-}
type alias ConcretePortableComponent data userdata tar msg =
    { init : Env () userdata -> msg -> data
    , update : Env () userdata -> WorldEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), ( Env () userdata, Bool ) )
    , updaterec : Env () userdata -> msg -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), Env () userdata )
    , view : Env () userdata -> data -> ( Renderable, Int )
    , matcher : data -> tar -> Bool
    }


{-| Translate a `ConcretePortableComponent` to a `ConcreteUserComponent`.

This will add an empty basedata (unit) and upcast target and messages to the generalized type.

-}
translatePortableComponent : ConcretePortableComponent data userdata tar msg -> PortableMsgCodec msg gmsg -> PortableTarCodec tar gtar -> ConcreteUserComponent data () userdata gtar gmsg () ()
translatePortableComponent pcomp msgcodec tarcodec =
    let
        msgMDecoder =
            genMsgDecoder msgcodec tarcodec
    in
    { init = \env gmsg -> ( pcomp.init env <| msgcodec.encode gmsg, () )
    , update =
        \env evt data () ->
            let
                ( resData, resMsg, resEnv ) =
                    pcomp.update env evt data
            in
            ( ( resData, () ), List.map msgMDecoder resMsg, resEnv )
    , updaterec =
        \env gmsg data () ->
            let
                ( resData, resMsg, resEnv ) =
                    pcomp.updaterec env (msgcodec.encode gmsg) data
            in
            ( ( resData, () ), List.map msgMDecoder resMsg, resEnv )
    , view = \env data () -> pcomp.view env data
    , matcher = \data () gtar -> pcomp.matcher data <| tarcodec.encode gtar
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


type alias AbstractComponent cdata userdata tar msg bdata scenemsg =
    AbstractGeneralModel (Env cdata userdata) WorldEvent tar msg ( Renderable, Int ) bdata (SceneOutputMsg scenemsg userdata)


type alias AbstractPortableComponent userdata tar msg =
    AbstractComponent () userdata tar msg () ()


genComponent : ConcreteUserComponent data cdata userdata tar msg bdata scenemsg -> Env cdata userdata -> msg -> AbstractComponent cdata userdata tar msg bdata scenemsg
genComponent concomp =
    abstract concomp


updateComponents : Env cdata userdata -> WorldEvent -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> ( List (AbstractComponent cdata userdata tar msg bdata scenemsg), List (MsgBase msg (SceneOutputMsg scenemsg userdata)), ( Env cdata userdata, Bool ) )
updateComponents env evt comps =
    updateObjects env evt comps


updatePortableComponents : Env cdata userdata -> WorldEvent -> List (AbstractPortableComponent userdata tar msg) -> ( List (AbstractPortableComponent userdata tar msg), List (MsgBase msg (SceneOutputMsg scenemsg userdata)), ( Env cdata userdata, Bool ) )
updatePortableComponents env evt pcomps =
    let
        ( newpcomps, newMsg, ( newEnv, newBlock ) ) =
            updateObjects (noCommonData env) evt pcomps

        newEnvC =
            addCommonData env.commonData newEnv

        newMsgfilterd =
            List.filterMap addSceneMsgtoPortable newMsg
    in
    ( newpcomps, newMsgfilterd, ( newEnvC, newBlock ) )


updateComponentsWithTarget : Env cdata userdata -> List (Msg tar msg (SceneOutputMsg scenemsg userdata)) -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> ( List (AbstractComponent cdata userdata tar msg bdata scenemsg), List (MsgBase msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )
updateComponentsWithTarget env msgs comps =
    updateObjectsWithTarget env msgs comps


updatePortableComponentsWithTarget : Env cdata userdata -> List (Msg tar msg (SceneOutputMsg () userdata)) -> List (AbstractPortableComponent userdata tar msg) -> ( List (AbstractPortableComponent userdata tar msg), List (MsgBase msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )
updatePortableComponentsWithTarget env msgs pcomps =
    let
        ( newpcomps, newMsg, newEnv ) =
            updateObjectsWithTarget (noCommonData env) msgs pcomps

        newEnvC =
            addCommonData env.commonData newEnv

        newMsgfilterd =
            List.filterMap addSceneMsgtoPortable newMsg
    in
    ( newpcomps, newMsgfilterd, newEnvC )


genComponentsRenderList : Env cdata userdata -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> List ( Renderable, Int )
genComponentsRenderList env compls =
    List.map (\comp -> (unroll comp).view env) compls


viewComponentsRenderList : List ( Renderable, Int ) -> Renderable
viewComponentsRenderList previews =
    group [] <|
        List.map (\( r, _ ) -> r) <|
            List.sortBy (\( _, n ) -> n) previews


viewComponents : Env cdata userdata -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> Renderable
viewComponents env compls =
    viewComponentsRenderList <| genComponentsRenderList env compls
