module Messenger.Layer.Layer exposing
    ( ConcreteLayer, AbstractLayer
    , genLayer
    , Handler
    , handleComponentMsgs
    , LayerInit, LayerUpdate, LayerUpdateRec, LayerView
    , LayerStorage
    )

{-|


# Layer

Gerneral Model and Helper functions for Layers.

@docs ConcreteLayer, AbstractLayer


## Generate

@docs genLayer


## Update

@docs Handler
@docs handleComponentMsgs
@docs LayerInit, LayerUpdate, LayerUpdateRec, LayerView
@docs LayerStorage

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.GeneralModel exposing (Matcher, abstract)
import Messenger.Scene.Scene exposing (MAbstractGeneralModel, MConcreteGeneralModel, MMsg, MMsgBase)


{-| init type sugar
-}
type alias LayerInit cdata userdata msg data =
    Env cdata userdata -> msg -> data


{-| update type sugar
-}
type alias LayerUpdate cdata userdata tar msg scenemsg data =
    Env cdata userdata -> UserEvent -> data -> ( data, List (MMsg tar msg scenemsg userdata), ( Env cdata userdata, Bool ) )


{-| updaterec type sugar
-}
type alias LayerUpdateRec cdata userdata tar msg scenemsg data =
    Env cdata userdata -> msg -> data -> ( data, List (MMsg tar msg scenemsg userdata), Env cdata userdata )


{-| view type sugar
-}
type alias LayerView cdata userdata data =
    Env cdata userdata -> data -> Renderable


{-| Layer Storage
-}
type alias LayerStorage cdata userdata tar msg scenemsg =
    msg -> Env cdata userdata -> AbstractLayer cdata userdata tar msg scenemsg


{-| Concrete Layer Model

Users deal with the fields in concrete model.

-}
type alias ConcreteLayer data cdata userdata tar msg scenemsg =
    { init : LayerInit cdata userdata msg data
    , update : LayerUpdate cdata userdata tar msg scenemsg data
    , updaterec : LayerUpdateRec cdata userdata tar msg scenemsg data
    , view : LayerView cdata userdata data
    , matcher : Matcher data tar
    }


{-| Abstract Layer Model.

Cannot be directedly modified.
Used for storage.

-}
type alias AbstractLayer cdata userdata tar msg scenemsg =
    MAbstractGeneralModel cdata userdata tar msg () scenemsg


{-| Generate abstract layer by a concrete layer.

Initialize it with env and msg.

-}
genLayer : ConcreteLayer data cdata userdata tar msg scenemsg -> LayerStorage cdata userdata tar msg scenemsg
genLayer conlayer =
    abstract <| addEmptyBData conlayer


{-| Handler Type.

A handler is used to handle the Component Msg sent to the layer.

**Make handler for every type of component msg.**

-}
type alias Handler data cdata userdata tar msg scenemsg cmsg =
    Env cdata userdata -> MMsgBase cmsg scenemsg userdata -> data -> ( data, List (MMsg tar msg scenemsg userdata), Env cdata userdata )


{-| Handle a list of component msgs.

**Note that for several list of component msgs with different types, you may have to use this function repeatedly.**

-}
handleComponentMsgs : Env cdata userdata -> List (MMsgBase cmsg scenemsg userdata) -> data -> List (MMsg tar msg scenemsg userdata) -> Handler data cdata userdata tar msg scenemsg cmsg -> ( data, List (MMsg tar msg scenemsg userdata), Env cdata userdata )
handleComponentMsgs lastEnv compMsgs lastData lastLayerMsgs handler =
    List.foldl
        (\cm ( d, m, e ) ->
            let
                ( d2, m2, e2 ) =
                    handler e cm d
            in
            ( d2, m ++ m2, e2 )
        )
        ( lastData, lastLayerMsgs, lastEnv )
        compMsgs


{-| Turn layer into a general model.
-}
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
