module Messenger.Component.GlobalComponent exposing
    ( GCMsg, GCTarget
    , GlobalMsgCodec
    , GlobalTarCodec
    , genGlobalComponent
    , GlobalComponentStorage
    )

{-|


# Global components

@docs GCMsg, GCTarget
@docs GlobalMsgCodec
@docs GlobalTarCodec
@docs genGlobalComponent
@docs GlobalComponentStorage

-}

import Json.Encode exposing (Value)
import Messenger.Base exposing (Env)
import Messenger.GeneralModel exposing (Msg(..), MsgBase(..))


{-| Global component message type.
-}
type alias GCMsg =
    Value


{-| Global component target type.
-}
type alias GCTarget =
    String


{-| Global component storage type.
-}
type alias GlobalComponentStorage cdata userdata scenemsg =
    Env cdata userdata -> AbstractLayer cdata userdata GCTarget GCMsg scenemsg


{-| Translate a `ConcreteGlobalComponent` to a specific `ConcreteLayer`.
-}
translateGlobalComponent : ConcreteLayer data cdata userdata tar msg scenemsg -> GlobalTarCodec tar -> GlobalMsgCodec msg -> ConcreteLayer data cdata userdata GCTarget GCMsg scenemsg
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
genGlobalComponent : ConcreteLayer data cdata userdata tar msg scenemsg -> GlobalTarCodec tar -> GlobalMsgCodec msg -> GCMsg -> GlobalComponentStorage cdata userdata scenemsg
genGlobalComponent conpcomp tcodec mcodec gcmsg =
    genLayer (translateGlobalComponent conpcomp tcodec mcodec) gcmsg
