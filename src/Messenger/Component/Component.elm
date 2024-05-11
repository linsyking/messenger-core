module Messenger.Component.Component exposing
    ( AbstractComponent
    , AbstractPortableComponent
    , ConcretePortableComponent
    , ConcreteUserComponent
    , PortableMsgCodec
    , PortableTarCodec
    , addSceneMsgtoSOM
    , genComponent
    , genComponentsRenderList
    , translatePortableComponent
    , updateComponents
    , updateComponentsWithTarget
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


## User components

These are components that you users can create with custom **basedata**.

-}

import Canvas exposing (Renderable, group)
import Messenger.Base exposing (Env, WorldEvent)
import Messenger.GeneralModel exposing (AbstractGeneralModel, ConcreteGeneralModel, Msg(..), MsgBase(..), abstract, unroll)
import Messenger.Recursion exposing (updateObjects, updateObjectsWithTarget)
import Messenger.Scene.Scene exposing (SceneOutputMsg(..))


type alias ConcreteUserComponent data cdata userdata tar msg bdata scenemsg =
    ConcreteGeneralModel data (Env cdata userdata) WorldEvent tar msg ( Renderable, Int ) bdata (SceneOutputMsg scenemsg userdata)


type alias ConcretePortableComponent data userdata tar msg =
    { init : Env () userdata -> msg -> data
    , update : Env () userdata -> WorldEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), ( Env () userdata, Bool ) )
    , updaterec : Env () userdata -> msg -> data -> ( data, List (Msg tar msg (SceneOutputMsg () userdata)), Env () userdata )
    , view : Env () userdata -> data -> ( Renderable, Int )
    , matcher : data -> tar -> Bool
    }


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


type alias MsgDecoder specifictar specificmsg generaltar generalmsg som =
    Msg specifictar specificmsg som -> Msg generaltar generalmsg som


type alias PortableMsgCodec specificmsg generalmsg =
    { encode : generalmsg -> specificmsg
    , decode : specificmsg -> generalmsg
    }


type alias PortableTarCodec specifictar generaltar =
    { encode : generaltar -> specifictar
    , decode : specifictar -> generaltar
    }


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


addSceneMsgtoSOM : SceneOutputMsg () userdata -> Maybe (SceneOutputMsg scenemsg userdata)
addSceneMsgtoSOM sommsg =
    case sommsg of
        SOMChangeScene _ ->
            Nothing

        SOMPlayAudio n u o ->
            Just (SOMPlayAudio n u o)

        SOMAlert a ->
            Just (SOMAlert a)

        SOMStopAudio n ->
            Just (SOMStopAudio n)

        SOMSetVolume v ->
            Just (SOMSetVolume v)

        SOMPrompt n t ->
            Just (SOMPrompt n t)

        SOMSaveUserData ->
            Just SOMSaveUserData


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


updateComponentsWithTarget : Env cdata userdata -> List (Msg tar msg (SceneOutputMsg scenemsg userdata)) -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> ( List (AbstractComponent cdata userdata tar msg bdata scenemsg), List (MsgBase msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )
updateComponentsWithTarget env msgs comps =
    updateObjectsWithTarget env msgs comps


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
