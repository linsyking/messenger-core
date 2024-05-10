module Messenger.Layer.Layer exposing (..)

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, WorldEvent)
import Messenger.GeneralModel exposing (MAbstractGeneralModel, MConcreteGeneralModel, Msg, abstract)
import Messenger.Scene.Scene exposing (SceneOutputMsg)


type alias ConcreteLayer data cdata userdata tar msg scenemsg =
    { init : Env cdata userdata -> msg -> data
    , update : Env cdata userdata -> WorldEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), ( Env cdata userdata, Bool ) )
    , updaterec : Env cdata userdata -> msg -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )
    , view : Env cdata userdata -> data -> Renderable
    , matcher : data -> tar -> Bool
    }


type alias AbstractLayer cdata userdata tar msg scenemsg =
    MAbstractGeneralModel cdata userdata tar msg () scenemsg


genLayer : ConcreteLayer data cdata userdata tar msg scenemsg -> Env cdata userdata -> msg -> AbstractLayer cdata userdata tar msg scenemsg
genLayer conlayer =
    abstract <| addEmptyBData conlayer


addEmptyBData : ConcreteLayer data cdata userdata tar msg scenemsg -> MConcreteGeneralModel data cdata userdata tar msg () scenemsg
addEmptyBData mconnoB =
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
    , matcher = \data () tar -> mconnoB.matcher data tar
    }
