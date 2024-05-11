module Messenger.Layer.Layer exposing
    ( ConcreteLayer, AbstractLayer
    , genLayer
    , BasicUpdater, Distributor, Handler
    , handleComponentMsgs
    )

{-|


# Layer

Gerneral Model and Helper functions for Layers.

@docs ConcreteLayer, AbstractLayer


## Generate

@docs genLayer


## Update

@docs BasicUpdater, Distributor, Handler
@docs handleComponentMsgs

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (Env, WorldEvent)
import Messenger.GeneralModel exposing (MAbstractGeneralModel, MConcreteGeneralModel, Msg, MsgBase, abstract)
import Messenger.Scene.Scene exposing (SceneOutputMsg)


{-| Concrete Layer Model

Users deal with the fields in concrete model.

-}
type alias ConcreteLayer data cdata userdata tar msg scenemsg =
    { init : Env cdata userdata -> msg -> data
    , update : Env cdata userdata -> WorldEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), ( Env cdata userdata, Bool ) )
    , updaterec : Env cdata userdata -> msg -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )
    , view : Env cdata userdata -> data -> Renderable
    , matcher : data -> tar -> Bool
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
genLayer : ConcreteLayer data cdata userdata tar msg scenemsg -> Env cdata userdata -> msg -> AbstractLayer cdata userdata tar msg scenemsg
genLayer conlayer =
    abstract <| addEmptyBData conlayer


{-| Basic Update Type

A basic updater type used to update the basic data of the layer with event.

Users can use it as the first step of the update process

-}
type alias BasicUpdater data cdata userdata tar msg scenemsg =
    Env cdata userdata -> WorldEvent -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), ( Env cdata userdata, Bool ) )


{-| Distributor Type

A distributor is used to generate several list of Component Msgs for corresponding components list.

The `cmsgpacker` is a custom type to store the component msgs and their targets.

-}
type alias Distributor data cdata userdata tar msg scenemsg cmsgpacker =
    Env cdata userdata -> WorldEvent -> data -> ( data, ( List (Msg tar msg (SceneOutputMsg scenemsg userdata)), cmsgpacker ), Env cdata userdata )


{-| Handler Type

A handler is used to handle the Component Msg sent to the layer.

**Make handler for every type of component msg**

-}
type alias Handler data cdata userdata tar msg scenemsg cmsg =
    Env cdata userdata -> MsgBase cmsg (SceneOutputMsg scenemsg userdata) -> data -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )


{-| Handle a list of component msgs

**Note that for several list of component msgs with different types, you may have to use this function repeatedly**

-}
handleComponentMsgs : Env cdata userdata -> List (MsgBase cmsg (SceneOutputMsg scenemsg userdata)) -> data -> List (Msg tar msg (SceneOutputMsg scenemsg userdata)) -> Handler data cdata userdata tar msg scenemsg cmsg -> ( data, List (Msg tar msg (SceneOutputMsg scenemsg userdata)), Env cdata userdata )
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


{-| transfer layer into a general model
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
