module Messenger.Layer.Layer exposing
    ( ConcreteLayer, AbstractLayer
    , genLayer
    , Handler
    , handleComponentMsgs
    , LayerInit, LayerUpdate, LayerUpdateRec, LayerView
    , LayerStorage
    , LayerUpdateMiddleStep, LayerUpdateRecMiddleStep
    )

{-|


# Layer

General Model and Helper functions for Layers.

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
The initializing function for layer. It receives the environment, the initializing message, and returns the layer's data.
-}
type alias LayerInit cdata userdata msg data =
    Env cdata userdata -> msg -> data


{-| update type sugar

Determines the behaviour when a UserEvent is sent to the layer. This could include, but not limited to, a mouse click, a keyboard press, or most commonly a time tick
(which means a specific amount of time has passed, see the manual). In most cases you should distribute the events to the components belonging to the group.

Receives the environment env, the UserEvent evnt, the currect data of the layer ,and returns a structure in (Data ,List Msg, (Env, Bool)).

  - Note: The last bool determines if the message would be blocked from the next layer. If it is set to be True, then all the layers behind it(generated after it
    in the initializing of scene) will not receive this UserEvent. This is useful when you want the action of the player only influence the front layers, but not the layers behind them.
  - Note: This function is also the place where you should handle the routine chores of the layers and collect component message. For more information please read the 5-step update framework in the manual.

-}
type alias LayerUpdate cdata userdata tar msg scenemsg data =
    Env cdata userdata -> UserEvent -> data -> ( data, List (MMsg tar msg scenemsg userdata), ( Env cdata userdata, Bool ) )


{-| updaterec type sugar

Determines the behaviour when other components or layers sends messages to the layer. The layer then use the message to change its state, initializing components and/or send relavent message to its component.

Receives the environment env, the layer messages (defined in LayerBase.elm), the currect data, and returns a structure in (Data,List Msg, Env).

-}
type alias LayerUpdateRec cdata userdata tar msg scenemsg data =
    Env cdata userdata -> msg -> data -> ( data, List (MMsg tar msg scenemsg userdata), Env cdata userdata )


{-| view type sugar
Renders the layer. Note that there is no need for you to render the components of the layer. Instead, you can render the background of the map or the decoration of the windows here.

Receives the environment and the data of the layer.

-}
type alias LayerView cdata userdata data =
    Env cdata userdata -> data -> Renderable


{-| This is a type sugar for dividing a long update function into smaller pieces to enhance code quality. It receives a ((Data,BaseData),List Msg, (Env,bool)) and returns a tuple of same kind.
It is really similar to UpdateMiddleStep(which is in Component.elm), see the examples there.
-}
type alias LayerUpdateMiddleStep cdata data userdata scenemsg tar msg =
    ( data, List (MMsg tar msg scenemsg userdata), ( Env cdata userdata, Bool ) ) -> ( data, List (MMsg tar msg scenemsg userdata), ( Env cdata userdata, Bool ) )


{-| This is a type sugar for dividing a long updateRec function into smaller pieces to enhance code quality. It receives a ((Data,BaseData),List Msg, Env) and returns a tuple of same kind.
It is really similar to UpdateMiddleStep(which is in Component.elm), see the examples there.
-}
type alias LayerUpdateRecMiddleStep cdata data userdata scenemsg tar msg =
    ( data, List (MMsg tar msg scenemsg userdata), Env cdata userdata ) -> ( data, List (MMsg tar msg scenemsg userdata), Env cdata userdata )


{-| Layer Storage

Not very likely to be used. The messenger template will handle this for you automatically.

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
Not very likely to be used. The messenger template will handle this for you automatically.

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

  - Note its main use is to deal with all the component msgs with the component message handler function (which is designed to handle one message) provided by the user. In real case just
    determine the handler function and put it as a parameter of this function.

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
Not very likely to be used. The messenger template will handle this for you automatically.
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
